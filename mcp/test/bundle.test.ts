import { describe, expect, it } from "vitest";
import { loadBundle } from "../src/bundle.js";
import type { Bundle } from "../src/types.js";

const validBundle: Bundle = {
  meta: { siteVersion: "", bundleVersion: 1, generatedAt: "2026-10-15T00:00:00Z", license: "CC-BY-NC-4.0" },
  categories: [{ name: "Distribution", nameZh: "分布", count: 1 }],
  charts: [{
    id: "ViolinPlot", name: "Basic Violin Plot", nameZh: null,
    type: "Violin Plot", level1: "BASICS", level2: "Distribution",
    tutorialUrl: "https://site/Distribution/ViolinPlot.html", tutorialUrlZh: null,
    imageUrl: "https://img/1.png", description: "A plot, with comma", codeRefs: ["r"],
  }],
  codeSnippets: [{
    chartId: "ViolinPlot", lang: "r", code: "ggplot(df) + geom_violin()",
    packages: ["ggplot2"], dataUrls: [], sourceAnchor: "https://site/Distribution/ViolinPlot.html",
  }],
  keywords: { synonyms: {}, intentKeywords: {} },
  citation: {
    paperDoi: "10.1002/imm3.70038", paperUrl: "https://arxiv.org/abs/2503.06845",
    bibtex: "@article{wang2025bizard,...}", license: "CC-BY-NC-4.0",
  },
};

describe("loadBundle", () => {
  it("accepts a valid bundle", () => {
    expect(loadBundle(validBundle).charts).toHaveLength(1);
  });

  it("rejects a bundle missing charts", () => {
    const bad = { ...validBundle } as Partial<Bundle>;
    delete bad.charts;
    expect(() => loadBundle(bad)).toThrow(/missing or invalid/i);
  });

  it("rejects a chart with no id", () => {
    const bad = structuredClone(validBundle);
    // @ts-expect-error corrupting fixture
    delete bad.charts[0].id;
    expect(() => loadBundle(bad)).toThrow(/missing or invalid/i);
  });

  it.each(["name", "type", "level1", "level2", "tutorialUrl", "imageUrl", "description"] as const)(
    "rejects a chart whose %s is null (builder restval pass-through)",
    (field) => {
      const bad = structuredClone(validBundle);
      // @ts-expect-error corrupting fixture to simulate a malformed bundle
      bad.charts[0][field] = null;
      expect(() => loadBundle(bad)).toThrow(/missing or invalid/i);
    },
  );
});
