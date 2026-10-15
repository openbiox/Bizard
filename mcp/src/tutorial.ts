import type { Bundle, Chart, CodeSnippet } from "./types.js";

const LICENSE_NOTE =
  "CC-BY-NC-4.0: non-commercial use with attribution. Cite Bizard (see bizard_cite).";

export function findChart(bundle: Bundle, id: string): Chart | undefined {
  const lower = id.toLowerCase();
  return bundle.charts.find((c) => c.id.toLowerCase() === lower);
}

export function snippetsFor(bundle: Bundle, chartId: string): CodeSnippet[] {
  return bundle.codeSnippets.filter((s) => s.chartId === chartId);
}

export function tutorialPayload(bundle: Bundle, chartId: string) {
  const chart = findChart(bundle, chartId);
  if (!chart) throw new Error(`Unknown chartId: ${chartId}`);
  const snips = snippetsFor(bundle, chart.id);
  return {
    id: chart.id,
    name: chart.name,
    nameZh: chart.nameZh,
    description: chart.description,
    tutorialUrl: chart.tutorialUrl,
    tutorialUrlZh: chart.tutorialUrlZh,
    imageUrl: chart.imageUrl,
    packages: [...new Set(snips.flatMap((s) => s.packages))],
    dataUrls: [...new Set(snips.flatMap((s) => s.dataUrls))],
    languages: chart.codeRefs,
    snippetCount: snips.length,
  };
}

export function codePayload(bundle: Bundle, chartId: string, language: "r" | "python" | "julia") {
  const chart = findChart(bundle, chartId);
  if (!chart) throw new Error(`Unknown chartId: ${chartId}`);
  const snips = snippetsFor(bundle, chart.id).filter((s) => s.lang === language);
  if (chart.codeRefs.length === 0) {
    return {
      code: null,
      packages: [],
      dataUrls: [],
      license: LICENSE_NOTE,
      message: `This tutorial has no structured code. Full tutorial: ${chart.tutorialUrl}`,
      tutorialUrl: chart.tutorialUrl,
    };
  }
  if (snips.length === 0) {
    throw new Error(
      `No ${language} code for ${chartId}. Available languages: ${chart.codeRefs.join(", ")}.`
    );
  }
  return {
    code: snips.map((s) => s.code).join("\n\n"),
    packages: [...new Set(snips.flatMap((s) => s.packages))],
    dataUrls: [...new Set(snips.flatMap((s) => s.dataUrls))],
    license: LICENSE_NOTE,
    message: null,
  };
}
