import type { Bundle } from "../src/types.js";

export const validBundle: Bundle = {
  meta: { siteVersion: "", bundleVersion: 1, generatedAt: "2026-10-15T00:00:00Z", license: "CC-BY-NC-4.0" },
  categories: [{ name: "Distribution", nameZh: "分布", count: 1 }],
  charts: [
    {
      id: "ViolinPlot", name: "Basic Violin Plot", nameZh: null,
      type: "Violin Plot", level1: "BASICS", level2: "Distribution",
      tutorialUrl: "https://site/Distribution/ViolinPlot.html", tutorialUrlZh: null,
      imageUrl: "https://img/1.png", description: "Violin distribution plot", codeRefs: ["r"],
    },
    {
      id: "BoxPlot", name: "Box Plot", nameZh: "箱线图",
      type: "Box Plot", level1: "BASICS", level2: "Distribution",
      tutorialUrl: "https://site/Distribution/BoxPlot.html", tutorialUrlZh: null,
      imageUrl: "https://img/2.png", description: "Boxplot comparison", codeRefs: [],
    },
  ],
  codeSnippets: [{
    chartId: "ViolinPlot", lang: "r", code: "library(ggplot2)\nggplot(df, aes(x)) + geom_violin()",
    packages: ["ggplot2"], dataUrls: [], sourceAnchor: "https://site/Distribution/ViolinPlot.html",
  }],
  keywords: {
    synonyms: { violin: ["violin plot", "小提琴"] },
    intentKeywords: { "show distribution": ["violin", "boxplot", "小提琴", "箱线"] },
  },
  citation: {
    paperDoi: "10.1002/imm3.70038", paperUrl: "https://arxiv.org/abs/2503.06845",
    bibtex: "@article{wang2025bizard,...}", license: "CC-BY-NC-4.0",
  },
};
