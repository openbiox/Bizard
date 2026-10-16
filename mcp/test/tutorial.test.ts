import { describe, expect, it } from "vitest";
import { tutorialPayload, codePayload, findChart, snippetsFor } from "../src/tutorial.js";
import { validBundle } from "./fixtures.js";

describe("findChart", () => {
  it("finds by exact id", () => {
    expect(findChart(validBundle, "ViolinPlot")?.id).toBe("ViolinPlot");
  });
  it("returns undefined for unknown id", () => {
    expect(findChart(validBundle, "nope")).toBeUndefined();
  });
});

describe("tutorialPayload", () => {
  it("includes setup packages, data URLs and tutorial URL", () => {
    const t = tutorialPayload(validBundle, "ViolinPlot");
    expect(t.packages).toContain("ggplot2");
    expect(t.dataUrls).toEqual([]);
    expect(t.tutorialUrl).toContain("ViolinPlot.html");
  });
});

describe("codePayload", () => {
  it("returns code with packages for a known chart+language", () => {
    const c = codePayload(validBundle, "ViolinPlot", "r");
    expect(c.code).toContain("geom_violin");
    expect(c.packages).toContain("ggplot2");
    expect(c.license).toContain("CC-BY-NC-4.0");
  });

  it("degrades gracefully when the tutorial has no structured code", () => {
    const c = codePayload(validBundle, "BoxPlot", "r");
    expect(c.code).toBeNull();
    expect(c.message).toMatch(/no structured code/i);
    expect(c.tutorialUrl).toContain("BoxPlot.html");
  });

  it("errors on language with no snippets", () => {
    expect(() => codePayload(validBundle, "ViolinPlot", "julia")).toThrow(/no julia code/i);
  });
});
