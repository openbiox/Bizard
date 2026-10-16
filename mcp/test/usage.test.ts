import { describe, expect, it } from "vitest";
import { readFile, rm } from "node:fs/promises";
import { recordUsage, readUsage } from "../src/usage.js";

describe("usage log", () => {
  it("appends JSONL and hashes queries", async () => {
    const log = "/tmp/vb-test.jsonl";
    await rm(log, { force: true });
    await recordUsage({ event: "search", chartId: "ViolinPlot", language: "en", query: "violin plot" }, log);
    const lines = (await readFile(log, "utf-8")).trim().split("\n");
    expect(lines).toHaveLength(1);
    const row = JSON.parse(lines[0]);
    expect(row.chartId).toBe("ViolinPlot");
    expect(row.queryHash).toMatch(/^[0-9a-f]{8}$/);
    expect(JSON.stringify(row)).not.toContain("violin plot");
  });

  it("respects BIZARD_USAGE_LOG=off", async () => {
    const prev = process.env.BIZARD_USAGE_LOG;
    process.env.BIZARD_USAGE_LOG = "off";
    const res = await recordUsage({ event: "view", chartId: "X" }, "/tmp/vb-off.jsonl");
    expect(res).toBe(false);
    if (prev === undefined) delete process.env.BIZARD_USAGE_LOG; else process.env.BIZARD_USAGE_LOG = prev;
  });

  it("readUsage returns parsed rows from a custom log", async () => {
    const log = "/tmp/vb-read.jsonl";
    await rm(log, { force: true });
    await recordUsage({ event: "adapt", chartId: "BoxPlot" }, log);
    const rows = await readUsage(10, log);
    expect(rows).toHaveLength(1);
    expect(rows[0].chartId).toBe("BoxPlot");
  });
});
