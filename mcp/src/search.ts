import type { Bundle, Chart } from "./types.js";

export interface SearchHit {
  chart: Chart;
  score: number;
}

/** Tokenize a query into lowercase terms; CJK chars become individual terms. */
export function tokenize(query: string): string[] {
  return query
    .toLowerCase()
    .split(/[\s,，。;；:、!！?？()（）]+/)
    .map((t) => t.trim())
    .filter((t) => t.length > 0);
}

/**
 * Expand query terms via the bundle's synonym table (both directions:
 * canonical → synonyms and synonyms → canonical) for cross-language reach.
 */
export function expandTerms(query: string, synonyms: Record<string, string[]>): string[] {
  const terms = new Set<string>();
  for (const raw of tokenize(query)) {
    terms.add(raw);
    for (const [canon, syns] of Object.entries(synonyms)) {
      const all = [canon, ...syns].map((s) => s.toLowerCase());
      if (all.includes(raw)) {
        for (const s of all) terms.add(s);
      }
    }
  }
  return [...terms];
}

/** Null-safe lowercase-includes: a chart field lost to a malformed CSV row never crashes scoring. */
const contains = (field: string | null | undefined, term: string): boolean =>
  typeof field === "string" && field.toLowerCase().includes(term);

/**
 * Score charts against expanded terms: name matches weigh most, then type,
 * then description/levels. Deterministic; no LLM.
 */
export function searchCharts(bundle: Bundle, query: string, opts?: { category?: string }): SearchHit[] {
  const terms = expandTerms(query, bundle.keywords?.synonyms ?? {});
  const hits: SearchHit[] = [];
  for (const chart of bundle.charts) {
    if (opts?.category && opts.category !== chart.level2) continue;
    let score = 0;
    for (const term of terms) {
      if (contains(chart.name, term)) score += 5;
      if (contains(chart.nameZh, term)) score += 5;
      if (contains(chart.type, term)) score += 3;
      if (contains(chart.description, term)) score += 2;
      if (contains(chart.level2, term)) score += 3;
    }
    if (score > 0) hits.push({ chart, score });
  }
  return hits.sort((a, b) => b.score - a.score || a.chart.id.localeCompare(b.chart.id));
}

/** Zero-hit fallback: top-3 charts from the same Level1 category as best type guess. */
export function adjacentSuggestions(bundle: Bundle, query: string, limit = 3): Chart[] {
  const terms = expandTerms(query, bundle.keywords?.synonyms ?? {});
  const level1 = bundle.charts.find((c) =>
    terms.some((t) => contains(c.level2, t) || contains(c.type, t))
  )?.level1;
  if (!level1) return bundle.charts.slice(0, limit);
  return bundle.charts.filter((c) => c.level1 === level1).slice(0, limit);
}
