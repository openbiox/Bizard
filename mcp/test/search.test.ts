import { describe, expect, it } from "vitest";
import { adjacentSuggestions, searchCharts } from "../src/search.js";
import { validBundle } from "./fixtures.js";
import type { Bundle } from "../src/types.js";

/** A bundle where one chart lost required fields to a malformed CSV row. */
const corruptedBundle = (): Bundle => {
  const bad = structuredClone(validBundle);
  // @ts-expect-error simulating a builder null pass-through
  bad.charts[1].name = null;
  bad.charts[1].type = null;
  bad.charts[1].description = null;
  bad.charts[1].level2 = null;
  return bad;
};

describe("searchCharts with a corrupted chart", () => {
  it("does not throw when a chart's required fields are null", () => {
    const hits = searchCharts(corruptedBundle(), "violin");
    expect(hits.map((h) => h.chart.id)).toEqual(["ViolinPlot"]);
  });

  it("scores the healthy chart normally and skips null fields", () => {
    const hits = searchCharts(corruptedBundle(), "box");
    expect(hits).toEqual([]);
  });
});

describe("adjacentSuggestions with a corrupted chart", () => {
  it("does not throw and falls back to the first charts", () => {
    const suggestions = adjacentSuggestions(corruptedBundle(), "zzz-no-match");
    expect(suggestions.length).toBeGreaterThan(0);
  });
});
