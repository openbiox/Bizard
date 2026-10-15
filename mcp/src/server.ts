import { McpServer } from "@modelcontextprotocol/sdk/server/mcp.js";
import { z } from "zod";
import type { Bundle, Chart } from "./types.js";
import { adjacentSuggestions, searchCharts } from "./search.js";

const SEARCH_LIMIT_MAX = 20;
const QUERY_MAX_CHARS = 4000;
const NO_BUNDLE_ERROR =
  "bizard-mcp: bundle snapshot unavailable. The maintainer must run " +
  "`npm run build:bundle` (or CI) to generate src/snapshot/bundle.json, then rebuild.";

/** Anything that can hand back a Bundle or null (snapshot today, ETag re-check later). */
export interface BundleHolder {
  get(): Bundle | null;
}

function json(value: unknown) {
  return { content: [{ type: "text" as const, text: JSON.stringify(value, null, 2) }] };
}

function bundleError() {
  return {
    isError: true as const,
    content: [{ type: "text" as const, text: NO_BUNDLE_ERROR }],
  };
}

/** Constant server identity — never derives from bundle state. */
const SERVER_IDENTITY = { name: "bizard-mcp", version: "0.1.0" } as const;

export function createBizardServer(source: Bundle | BundleHolder | null): McpServer {
  const getBundle = (): Bundle | null => {
    if (source === null) return null;
    if ("get" in source && typeof source.get === "function") return source.get();
    return source as Bundle;
  };

  const server = new McpServer(SERVER_IDENTITY);

  server.registerTool("bizard_search", {
    title: "Search Bizard chart tutorials",
    description: "Search the Bizard atlas (800+ biomedical chart tutorials, EN/zh) by keyword. Returns chart cards with tutorial URLs and image previews.",
    inputSchema: {
      query: z.string().max(QUERY_MAX_CHARS).describe("Search keywords, English or Chinese"),
      language: z.enum(["en", "zh"]).default("en").describe("Preferred result language"),
      category: z.string().optional().describe("Filter by category (from bizard_list_categories)"),
      limit: z.number().int().min(1).max(SEARCH_LIMIT_MAX).default(5),
    },
    annotations: { readOnlyHint: true, openWorldHint: false },
  }, async ({ query, language, category, limit }) => {
    const bundle = getBundle();
    if (!bundle) return bundleError();
    return searchTool(bundle, query, language, category, limit);
  });

  server.registerTool("bizard_list_categories", {
    title: "List Bizard categories",
    description: "List all Bizard chart categories (bilingual) with per-category chart counts.",
    inputSchema: {},
    annotations: { readOnlyHint: true, openWorldHint: false },
  }, async () => {
    const bundle = getBundle();
    if (!bundle) return bundleError();
    return categoriesTool(bundle);
  });

  return server;
}

export function searchTool(bundle: Bundle, query: string, language: "en" | "zh",
                           category?: string, limit = 5) {
  const hits = searchCharts(bundle, query, { category });
  const nameOf = (c: Chart) => (language === "zh" ? c.nameZh ?? c.name : c.name);
  const urlOf = (c: Chart) => (language === "zh" ? c.tutorialUrlZh ?? c.tutorialUrl : c.tutorialUrl);
  const cards = hits.slice(0, limit).map(({ chart }) => ({
    id: chart.id,
    name: nameOf(chart),
    type: chart.type,
    imageUrl: chart.imageUrl,
    tutorialUrl: urlOf(chart),
    description: chart.description,
  }));
  if (cards.length > 0) return json({ results: cards });
  const near = adjacentSuggestions(bundle, query).map((c) => ({
    id: c.id, name: nameOf(c), tutorialUrl: urlOf(c),
  }));
  return json({ results: [], suggestions: near, note: "No exact matches. Nearby charts in the closest category:" });
}

function categoriesTool(bundle: Bundle) {
  return json(bundle.categories);
}
