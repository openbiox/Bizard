// Hosted-instance audit: same checks as the local HTTP audit, against the
// team-deployed endpoint. Read-only + level-3 contribute (server has no creds).
const BASE = process.env.MCP_BASE ?? "http://biotree.top:38123/bizard-mcp/mcp";
const results = [];
const check = (n, ok, d = "") => { results.push({ n, ok }); console.log(`${ok ? "PASS" : "FAIL"}  ${n}${d ? " — " + d : ""}`); };

async function req(body, extraHeaders = {}) {
  const r = await fetch(BASE, {
    method: "POST",
    headers: { "content-type": "application/json", accept: "application/json, text/event-stream", ...extraHeaders },
    body: JSON.stringify(body),
  });
  const ct = r.headers.get("content-type") ?? "";
  let data;
  if (ct.includes("text/event-stream")) {
    const text = await r.text();
    const lines = text.split("\n").filter(l => l.startsWith("data:"));
    data = lines.length ? JSON.parse(lines[lines.length - 1].slice(5).trim()) : null;
  } else {
    data = await r.json().catch(() => null);
  }
  return { status: r.status, headers: r.headers, data };
}

// 1. initialize + notifications
const t0 = Date.now();
const init = await req({ jsonrpc: "2.0", id: 1, method: "initialize", params: { protocolVersion: "2025-03-26", capabilities: {}, clientInfo: { name: "hosted-audit", version: "0" } } });
const latency = Date.now() - t0;
check("initialize: HTTP 200", init.status === 200, `${latency}ms`);
check("initialize: serverInfo = bizard-mcp", init.data?.result?.serverInfo?.name === "bizard-mcp", `version=${init.data?.result?.serverInfo?.version}`);
check("initialize: version matches latest npm", /^0\.1\.[23]$/.test(init.data?.result?.serverInfo?.version ?? ""), init.data?.result?.serverInfo?.version);
const sid = init.headers.get("mcp-session-id");
check("stateless: no session id issued", !sid);
const notify = await fetch(BASE, { method: "POST", headers: { "content-type": "application/json", accept: "application/json, text/event-stream" }, body: JSON.stringify({ jsonrpc: "2.0", method: "notifications/initialized" }) });
check("notifications/initialized: 202", notify.status === 202, `status=${notify.status}`);

// 2. tools/list
const list = await req({ jsonrpc: "2.0", id: 2, method: "tools/list", params: {} });
const tools = list.data?.result?.tools ?? [];
check("tools/list: 8 tools", tools.length === 8, tools.map(t => t.name).join(","));

// 3. bizard_search en + zh
const sEn = await req({ jsonrpc: "2.0", id: 3, method: "tools/call", params: { name: "bizard_search", arguments: { query: "volcano plot", language: "en", limit: 3 } } });
const sEnJson = JSON.parse(sEn.data?.result?.content?.[0]?.text ?? "{}");
check("search en: hits>0", (sEnJson.results?.length ?? 0) > 0, sEnJson.results?.[0]?.id);
check("search en: URLs -> openbiox.github.io/Bizard", (sEnJson.results ?? []).every(r => r.tutorialUrl.startsWith("https://openbiox.github.io/Bizard/")));
const sZh = await req({ jsonrpc: "2.0", id: 4, method: "tools/call", params: { name: "bizard_search", arguments: { query: "火山图", language: "zh", limit: 2 } } });
check("search zh: hits>0", (JSON.parse(sZh.data?.result?.content?.[0]?.text ?? "{}").results?.length ?? 0) > 0);

// 4. list_categories — verify the array fix is deployed here
const cats = await req({ jsonrpc: "2.0", id: 5, method: "tools/call", params: { name: "bizard_list_categories", arguments: {} } });
const catsJson = JSON.parse(cats.data?.result?.content?.[0]?.text ?? "null");
check("list_categories: JSON array (0.1.3 fix live?)", Array.isArray(catsJson), Array.isArray(catsJson) ? `len=${catsJson.length}` : `got ${typeof catsJson} (server still <0.1.3)`);

// 5. get_code
const chartId = sEnJson.results?.[0]?.id ?? "iVolcanoPlot-798";
const code = await req({ jsonrpc: "2.0", id: 6, method: "tools/call", params: { name: "bizard_get_code", arguments: { chartId, language: "r" } } });
const codeJson = JSON.parse(code.data?.result?.content?.[0]?.text ?? "{}");
check("get_code r: code present", typeof codeJson.code === "string" && codeJson.code.length > 50);

// 6. cite
const cite = await req({ jsonrpc: "2.0", id: 7, method: "tools/call", params: { name: "bizard_cite", arguments: { chartId, format: "bibtex" } } });
check("cite bibtex: license + paper", /CC-BY-NC/i.test(cite.data?.result?.content?.[0]?.text ?? ""));

// 7. contribute_submit — must degrade to level 3 (server holds zero credentials)
const sub = await req({ jsonrpc: "2.0", id: 8, method: "tools/call", params: { name: "bizard_contribute_submit", arguments: { contributionType: "fix", title: "HOSTED-AUDIT-IGNORE", body: "Hosted endpoint audit probe — not a real contribution." } } });
const subText = sub.data?.result?.content?.[0]?.text ?? "";
check("contribute_submit: level-3 (zero server creds)", /level\s*3/i.test(subText), subText.slice(0, 80).replace(/\n/g, " "));
check("contribute_submit: deep-link target correct", subText.includes("github.com/openbiox/Bizard"));

// 8. resources
const resCat = await req({ jsonrpc: "2.0", id: 9, method: "resources/read", params: { uri: "bizard://catalog" } });
check("resource catalog: readable", (resCat.data?.result?.contents?.[0]?.text?.length ?? 0) > 100);
const resLlms = await req({ jsonrpc: "2.0", id: 10, method: "resources/read", params: { uri: "bizard://llms-txt" } });
check("resource llms-txt: '# Bizard' header", (resLlms.data?.result?.contents?.[0]?.text ?? "").startsWith("# Bizard"));

// 9. rate limit present?
let rlHit = false, rlStatus = 200;
for (let i = 0; i < 70; i++) {
  const r = await fetch(BASE, { method: "POST", headers: { "content-type": "application/json", accept: "application/json, text/event-stream" }, body: JSON.stringify({ jsonrpc: "2.0", id: 200 + i, method: "tools/list", params: {} }) });
  rlStatus = r.status;
  if (r.status === 429) { rlHit = true; console.log(`  -> 429 after ${i + 1} rapid req, retry-after=${r.headers.get("retry-after")}`); break; }
}
check("rate limit: 429 observed", rlHit, `last=${rlStatus}`);

const fails = results.filter(r => !r.ok);
console.log(`\n=== HOSTED AUDIT (${BASE}): ${results.length - fails.length}/${results.length} passed ===`);
if (fails.length) console.log("FAILURES: " + fails.map(f => f.n).join("; "));
