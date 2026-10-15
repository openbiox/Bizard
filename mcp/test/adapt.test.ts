import { describe, expect, it } from "vitest";
import { adaptGuidance } from "../src/adapt.js";
import { validBundle } from "./fixtures.js";

describe("adaptGuidance", () => {
  it("returns the five-section checklist", () => {
    const g = adaptGuidance(validBundle, "ViolinPlot", "r");
    expect(g.sections.map((s) => s.title)).toEqual([
      "Data shape check",
      "Dependency preparation",
      "Parameter mapping",
      "Common failure modes",
      "Alternatives",
    ]);
  });

  it("failure modes are category-specific (distribution charts mention data size)", () => {
    const g = adaptGuidance(validBundle, "ViolinPlot", "r");
    const fm = g.sections.find((s) => s.title === "Common failure modes")!;
    expect(JSON.stringify(fm)).toMatch(/distribution/i);
  });

  it("alternatives come from the same level1 category and exclude the chart itself", () => {
    const g = adaptGuidance(validBundle, "ViolinPlot", "r");
    const alt = g.sections.find((s) => s.title === "Alternatives")!;
    expect(JSON.stringify(alt)).not.toContain("ViolinPlot");
  });

  it("includes user data context echo when provided", () => {
    const g = adaptGuidance(validBundle, "ViolinPlot", "r", "my data is a 3-column CSV");
    expect(g.userDataContext).toBe("my data is a 3-column CSV");
  });

  it("throws for unknown chartId", () => {
    expect(() => adaptGuidance(validBundle, "nope", "r")).toThrow(/Unknown chartId/);
  });
});
