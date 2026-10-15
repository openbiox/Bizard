export interface BundleMeta {
  siteVersion: string;
  bundleVersion: number;
  generatedAt: string;
  license: string;
}

export interface Category {
  name: string;
  nameZh: string | null;
  count: number;
}

export interface Chart {
  id: string;
  name: string;
  nameZh: string | null;
  type: string;
  level1: string;
  level2: string;
  tutorialUrl: string;
  tutorialUrlZh: string | null;
  imageUrl: string;
  description: string;
  codeRefs: string[];
}

export interface CodeSnippet {
  chartId: string;
  lang: "r" | "python" | "julia";
  code: string;
  packages: string[];
  dataUrls: string[];
  sourceAnchor: string;
}

export interface Citation {
  paperDoi: string;
  paperUrl: string;
  bibtex: string;
  license: string;
}

export interface Bundle {
  meta: BundleMeta;
  categories: Category[];
  charts: Chart[];
  codeSnippets: CodeSnippet[];
  keywords: { synonyms: Record<string, string[]>; intentKeywords: Record<string, string[]> } | null;
  citation: Citation;
}
