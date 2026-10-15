import { describe, expect, it } from "vitest";
import { createBizardServer } from "../src/server.js";
import { startHttpServer } from "../src/http.js";
import { validBundle } from "./fixtures.js";

const INIT = {
  jsonrpc: "2.0", id: 1, method: "initialize",
  params: {
    protocolVersion: "2025-06-18", capabilities: {},
    clientInfo: { name: "t", version: "0" },
  },
};

const INIT_HEADERS = {
  "Content-Type": "application/json",
  Accept: "application/json, text/event-stream",
};

function freePort(): number {
  return 18000 + Math.floor(Math.random() * 1000);
}

describe("http transport", () => {
  it("responds to initialize over Streamable HTTP", async () => {
    const port = freePort();
    const server = createBizardServer(validBundle);
    startHttpServer(server, port);
    const res = await fetch(`http://127.0.0.1:${port}/mcp`, {
      method: "POST", headers: INIT_HEADERS, body: JSON.stringify(INIT),
    });
    expect(res.status).toBe(200);
    const text = await res.text();
    expect(text).toContain("bizard");
  });

  it("answers tools/list after initialize with the 8 lifecycle tools", async () => {
    const port = freePort();
    const server = createBizardServer(validBundle);
    startHttpServer(server, port);

    const initRes = await fetch(`http://127.0.0.1:${port}/mcp`, {
      method: "POST", headers: INIT_HEADERS, body: JSON.stringify(INIT),
    });
    expect(initRes.status).toBe(200);
    const session = initRes.headers.get("mcp-session-id");

    const listRes = await fetch(`http://127.0.0.1:${port}/mcp`, {
      method: "POST",
      headers: { ...INIT_HEADERS, ...(session ? { "mcp-session-id": session } : {}) },
      body: JSON.stringify({ jsonrpc: "2.0", id: 2, method: "tools/list" }),
    });
    expect(listRes.status).toBe(200);
    const text = await listRes.text();
    expect(text).toContain("bizard_search");
    expect(text).toContain("bizard_contribute_check");
  });

  it("rate limits excessive requests from one IP", async () => {
    const port = freePort();
    const server = createBizardServer(validBundle);
    startHttpServer(server, port);
    // Default capacity is generous; hammer /mcp with a 404 path (cheap) and
    // expect some 429 among the first burst after capacity is consumed.
    let saw429 = false;
    for (let i = 0; i < 200; i++) {
      const res = await fetch(`http://127.0.0.1:${port}/mcp`, {
        method: "POST", headers: INIT_HEADERS, body: JSON.stringify(INIT),
      });
      await res.text().catch(() => "");
      if (res.status === 429) { saw429 = true; break; }
    }
    expect(saw429).toBe(true);
  });

  it("keys rate buckets on X-Forwarded-For when BIZARD_TRUST_PROXY is set (spec §6.2 前置反代)", async () => {
    const port = freePort();
    const server = createBizardServer(validBundle);
    process.env.BIZARD_TRUST_PROXY = "1";
    try {
      startHttpServer(server, port);
      // Exhaust client 9.9.9.9's bucket (capacity 60/min).
      let exhausted = false;
      for (let i = 0; i < 200; i++) {
        const res = await fetch(`http://127.0.0.1:${port}/mcp`, {
          method: "POST",
          headers: { ...INIT_HEADERS, "X-Forwarded-For": "9.9.9.9" },
          body: JSON.stringify(INIT),
        });
        await res.text().catch(() => "");
        if (res.status === 429) { exhausted = true; break; }
      }
      expect(exhausted).toBe(true);
      // A different forwarded client behind the same proxy socket is NOT limited.
      const other = await fetch(`http://127.0.0.1:${port}/mcp`, {
        method: "POST",
        headers: { ...INIT_HEADERS, "X-Forwarded-For": "8.8.8.8" },
        body: JSON.stringify(INIT),
      });
      await other.text();
      expect(other.status).toBe(200);
    } finally {
      delete process.env.BIZARD_TRUST_PROXY;
    }
  });

  it("ignores X-Forwarded-For by default so clients cannot spoof fresh buckets", async () => {
    const port = freePort();
    const server = createBizardServer(validBundle);
    startHttpServer(server, port);
    // Exhaust one forwarded identity, then "rotate" it — same socket bucket,
    // so the rotated identity must still be rate limited.
    let exhausted = false;
    for (let i = 0; i < 200; i++) {
      const res = await fetch(`http://127.0.0.1:${port}/mcp`, {
        method: "POST",
        headers: { ...INIT_HEADERS, "X-Forwarded-For": `spoofed-${i}` },
        body: JSON.stringify(INIT),
      });
      await res.text().catch(() => "");
      if (res.status === 429) { exhausted = true; break; }
    }
    expect(exhausted).toBe(true);
    const rotated = await fetch(`http://127.0.0.1:${port}/mcp`, {
      method: "POST",
      headers: { ...INIT_HEADERS, "X-Forwarded-For": "spoofed-fresh" },
      body: JSON.stringify(INIT),
    });
    await rotated.text();
    expect(rotated.status).toBe(429);
  });
});
