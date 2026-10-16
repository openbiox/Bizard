import { afterEach, describe, expect, it } from "vitest";
import { readFile, rm } from "node:fs/promises";
import { createBizardServer } from "../src/server.js";
import { callTool } from "./client.js";
import { validBundle } from "./fixtures.js";

const LOG = "/tmp/vb-wiring.jsonl";

const prevEnv = process.env.BIZARD_USAGE_LOG;

afterEach(async () => {
  await rm(LOG, { force: true });
  if (prevEnv === undefined) delete process.env.BIZARD_USAGE_LOG;
  else process.env.BIZARD_USAGE_LOG = prevEnv;
});

/** recordUsage is fire-and-forget from tool handlers; poll until every needle lands in the log. */
async function waitForRows(...needles: string[]): Promise<string> {
  for (let i = 0; i < 50; i++) {
    try {
      const txt = await readFile(LOG, "utf-8");
      if (needles.every((n) => txt.includes(n))) return txt;
    } catch { /* not written yet */ }
    await new Promise((r) => setTimeout(r, 20));
  }
  return await readFile(LOG, "utf-8").catch(() => "");
}

describe("usage wiring (README Privacy contract: tool calls append to the local log)", () => {
  it("bizard_search appends a search row with a hashed query and no chartId", async () => {
    process.env.BIZARD_USAGE_LOG = LOG;
    const server = createBizardServer(validBundle);
    await callTool(server, "bizard_search", { query: "violin", language: "en" });
    const txt = await waitForRows('"event":"search"', '"chartId":null');
    expect(txt).toContain('"event":"search"');
    expect(txt).toContain('"queryHash":');
    expect(txt).not.toContain("violin");
  });

  it("bizard_get_tutorial appends a view row", async () => {
    process.env.BIZARD_USAGE_LOG = LOG;
    const server = createBizardServer(validBundle);
    await callTool(server, "bizard_get_tutorial", { chartId: "ViolinPlot" });
    const txt = await waitForRows('"event":"view"', '"chartId":"ViolinPlot"');
    expect(txt).toContain('"event":"view"');
    expect(txt).toContain('"chartId":"ViolinPlot"');
  });

  it("bizard_adapt_guidance appends an adapt row and bizard_cite a cite row", async () => {
    process.env.BIZARD_USAGE_LOG = LOG;
    const server = createBizardServer(validBundle);
    await callTool(server, "bizard_adapt_guidance", { chartId: "ViolinPlot", language: "r" });
    await callTool(server, "bizard_cite", { chartId: "ViolinPlot" });
    const txt = await waitForRows('"event":"adapt"', '"event":"cite"');
    expect(txt).toContain('"event":"adapt"');
    expect(txt).toContain('"event":"cite"');
  });

  it("BIZARD_USAGE_LOG=off disables logging even through tool calls", async () => {
    process.env.BIZARD_USAGE_LOG = "off";
    const server = createBizardServer(validBundle);
    await callTool(server, "bizard_search", { query: "violin" });
    await new Promise((r) => setTimeout(r, 100));
    await expect(readFile(LOG, "utf-8")).rejects.toThrow();
  });
});
