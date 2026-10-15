import { describe, expect, it } from "vitest";
import { createBizardServer } from "../src/server.js";
import { callTool, listTools } from "./client.js";
import { validBundle } from "./fixtures.js";

describe("createBizardServer (stdio-capable core)", () => {
  it("lists the 6 initial tools", async () => {
    const tools = await listTools(createBizardServer(validBundle));
    expect(tools.map((t) => t.name).sort()).toEqual([
      "bizard_adapt_guidance", "bizard_cite", "bizard_get_code",
      "bizard_get_tutorial", "bizard_list_categories", "bizard_search",
    ]);
  });
});

describe("bizard_search", () => {
  it("returns matching charts with tutorialUrl", async () => {
    const server = createBizardServer(validBundle);
    const result = await callTool(server, "bizard_search", { query: "violin" });
    const text = result.content[0].text;
    expect(text).toContain("ViolinPlot");
    expect(text).toContain("tutorialUrl");
  });

  it("returns adjacent suggestions on zero hits", async () => {
    const server = createBizardServer(validBundle);
    const result = await callTool(server, "bizard_search", { query: "zzzznope" });
    expect(result.content[0].text).toContain("Nearby");
  });
});

describe("degraded mode (no bundle snapshot)", () => {
  it("returns a structured build hint error from every tool", async () => {
    const server = createBizardServer(null);
    const search = await callTool(server, "bizard_search", { query: "violin" });
    expect(search.isError).toBe(true);
    expect(search.content[0].text).toContain("npm run build:bundle");

    const cats = await callTool(server, "bizard_list_categories", {});
    expect(cats.isError).toBe(true);
    expect(cats.content[0].text).toContain("npm run build:bundle");
  });
});

describe("bizard_get_tutorial / bizard_get_code", () => {
  it("returns tutorial structure for a known chart", async () => {
    const server = createBizardServer(validBundle);
    const result = await callTool(server, "bizard_get_tutorial", { chartId: "ViolinPlot" });
    expect(result.content[0].text).toContain("tutorialUrl");
    expect(result.isError).toBeUndefined();
  });

  it("returns structured error for unknown chartId", async () => {
    const server = createBizardServer(validBundle);
    const result = await callTool(server, "bizard_get_tutorial", { chartId: "nope" });
    expect(result.isError).toBe(true);
    expect(result.content[0].text).toContain("Unknown chartId");
  });

  it("returns runnable code for known chart+language", async () => {
    const server = createBizardServer(validBundle);
    const result = await callTool(server, "bizard_get_code", { chartId: "ViolinPlot", language: "r" });
    expect(result.content[0].text).toContain("geom_violin");
  });

  it("degraded mode: get_tutorial returns structured build hint error", async () => {
    const server = createBizardServer(null);
    const result = await callTool(server, "bizard_get_tutorial", { chartId: "ViolinPlot" });
    expect(result.isError).toBe(true);
    expect(result.content[0].text).toContain("npm run build:bundle");
  });
});

describe("bizard_adapt_guidance / bizard_cite", () => {
  it("returns five-section checklist for a known chart", async () => {
    const server = createBizardServer(validBundle);
    const result = await callTool(server, "bizard_adapt_guidance", { chartId: "ViolinPlot", language: "r" });
    expect(result.content[0].text).toContain("Common failure modes");
  });

  it("cite returns bibtex with chart annotation", async () => {
    const server = createBizardServer(validBundle);
    const result = await callTool(server, "bizard_cite", { chartId: "ViolinPlot", format: "bibtex" });
    expect(result.content[0].text).toContain("ViolinPlot.html");
    expect(result.content[0].text).toContain("10.1002/imm3.70038");
  });

  it("degraded mode: adapt_guidance returns structured build hint error", async () => {
    const server = createBizardServer(null);
    const result = await callTool(server, "bizard_adapt_guidance", { chartId: "ViolinPlot", language: "r" });
    expect(result.isError).toBe(true);
    expect(result.content[0].text).toContain("npm run build:bundle");
  });
});
