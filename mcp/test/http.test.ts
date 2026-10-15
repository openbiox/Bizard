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
});
