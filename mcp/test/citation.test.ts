import { describe, expect, it } from "vitest";
import { citationPayload } from "../src/citation.js";
import { validBundle } from "./fixtures.js";

describe("citationPayload", () => {
  it("bibtex includes DOI and license for a chart", () => {
    const c = citationPayload(validBundle, "ViolinPlot", false, "bibtex");
    expect(c.text).toContain("10.1002/imm3.70038");
    expect(c.text).toContain("CC-BY-NC");
    expect(c.text).toContain("ViolinPlot.html");
  });

  it("paper-only mode omits chart URL", () => {
    const c = citationPayload(validBundle, null, true, "bibtex");
    expect(c.text).not.toContain(".html");
  });

  it("text format includes a paste-ready methods sentence", () => {
    const c = citationPayload(validBundle, "ViolinPlot", false, "text");
    expect(c.text).toContain("was generated following the Bizard tutorial");
  });

  it("json format returns structured paper+chart object", () => {
    const c = citationPayload(validBundle, "ViolinPlot", false, "json");
    const parsed = c.json as { paper: { doi: string }; chart: { id: string } | null };
    expect(parsed.paper.doi).toBe("10.1002/imm3.70038");
    expect(parsed.chart?.id).toBe("ViolinPlot");
  });

  it("throws for unknown chartId when not paper-only", () => {
    expect(() => citationPayload(validBundle, "nope", false, "bibtex")).toThrow(/Unknown chartId/);
  });
});
