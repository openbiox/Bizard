import type { Bundle } from "./types.js";
import { findChart } from "./tutorial.js";

const LICENSE_NOTE = "CC-BY-NC-4.0";

const METHODS_SENTENCE_EN =
  'The figure was generated following the Bizard tutorial "{name}" ({url}); Bizard is licensed CC-BY-NC-4.0.';
const METHODS_SENTENCE_ZH =
  "该图参考 Bizard 教程「{name}」({url}) 复现；Bizard 采用 CC-BY-NC-4.0 许可。";

export interface CitationResult {
  text: string;
  json: unknown;
}

export function citationPayload(
  bundle: Bundle, chartId: string | null, paperOnly: boolean, format: "bibtex" | "text" | "json",
): CitationResult {
  const cite = bundle.citation;
  const wantsChart = chartId !== null && !paperOnly;
  const chart = wantsChart ? findChart(bundle, chartId) : undefined;
  if (wantsChart && !chart) throw new Error(`Unknown chartId: ${chartId}`);

  if (format === "json") {
    return {
      text: "",
      json: {
        paper: { doi: cite.paperDoi, url: cite.paperUrl, license: cite.license },
        chart: chart ? { id: chart.id, name: chart.name, url: chart.tutorialUrl } : null,
      },
    };
  }
  if (format === "bibtex") {
    const text = chart
      ? `${cite.bibtex}\n% Chart: ${chart.name} — ${chart.tutorialUrl}\n% License: ${cite.license}`
      : cite.bibtex;
    return { text, json: null };
  }
  const text = chart
    ? fillMethods(METHODS_SENTENCE_EN, chart.name, chart.tutorialUrl)
      + "\n" + fillMethods(METHODS_SENTENCE_ZH, chart.name, chart.tutorialUrl)
    : `Biomedical charts adapted from Bizard (${cite.paperUrl}), DOI ${cite.paperDoi}, license ${cite.license}.`;
  return { text, json: null };
}

function fillMethods(template: string, name: string, url: string): string {
  return template.replaceAll("{name}", name).replaceAll("{url}", url);
}
