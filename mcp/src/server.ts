import { McpServer } from "@modelcontextprotocol/sdk/server/mcp.js";
import { z } from "zod";
import type { Bundle, Chart } from "./types.js";
import { adjacentSuggestions, searchCharts } from "./search.js";
import { registerTutorialTools } from "./tutorial-tools.js";
import { registerAdaptTools } from "./adapt-tools.js";
import { registerContributeTools } from "./contribute-tools.js";
import type { CredentialEnv } from "./contribute.js";
import { registerPrompts } from "./prompts.js";
import { registerResources } from "./resources.js";
import { updateNotice } from "./update-check.js";
import type { NoticeQueue } from "./update-check.js";

const SEARCH_LIMIT_MAX = 20;
const QUERY_MAX_CHARS = 4000;
export const NO_BUNDLE_ERROR =
  "bizard-mcp: bundle snapshot unavailable. The maintainer must run " +
  "`npm run build:bundle` (or CI) to generate src/snapshot/bundle.json, then rebuild.";

/** Anything that can hand back a Bundle or null (snapshot today, ETag re-check later). */
export interface BundleHolder {
  get(): Bundle | null;
}

function json(value: unknown, note?: string | null) {
  const payload: Record<string, unknown> = note === null || note === undefined
    ? { ...value as object }
    : { note, ...value as object };
  return {
    content: [{
      type: "text" as const,
      text: JSON.stringify(payload, null, 2),
    }],
  };
}

function bundleError() {
  return {
    isError: true as const,
    content: [{ type: "text" as const, text: NO_BUNDLE_ERROR }],
  };
}

/** Constant server identity — never derives from bundle state. */
const SERVER_IDENTITY = { name: "bizard-mcp", version: "0.1.0" } as const;

export function createBizardServer(
  source: Bundle | BundleHolder | null,
  notices?: NoticeQueue,
  opts?: { credentialEnv?: CredentialEnv },
): McpServer {
  const getBundle = (): Bundle | null => {
    if (source === null) return null;
    if ("get" in source && typeof source.get === "function") return source.get();
    return source as Bundle;
  };

  // One-time SFL notice (spec §3.4): consumed by the first tool result after an update swap.
  const drainNotice = (): string | null => (notices ? updateNotice(notices) : null);

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
    return searchTool(bundle, query, language, category, limit, drainNotice());
  });

  server.registerTool("bizard_list_categories", {
    title: "List Bizard categories",
    description: "List all Bizard chart categories (bilingual) with per-category chart counts.",
    inputSchema: {},
    annotations: { readOnlyHint: true, openWorldHint: false },
  }, async () => {
    const bundle = getBundle();
    if (!bundle) return bundleError();
    return categoriesTool(bundle, drainNotice());
  });

  const lazyBundle = {
    get: () => {
      const bundle = getBundle();
      if (!bundle) throw new Error(NO_BUNDLE_ERROR);
      return bundle;
    },
  };
  registerTutorialTools(server, lazyBundle);
  registerAdaptTools(server, lazyBundle);
  registerContributeTools(server, opts?.credentialEnv);
  registerPrompts(server);
  registerResources(server, getBundle);

  return server;
}

export function searchTool(bundle: Bundle, query: string, language: "en" | "zh",
                           category?: string, limit = 5, note: string | null = null) {
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
  if (cards.length > 0) return json({ results: cards }, note);
  const near = adjacentSuggestions(bundle, query).map((c) => ({
    id: c.id, name: nameOf(c), tutorialUrl: urlOf(c),
  }));
  return json({ results: [], suggestions: near, note: note ?? "No exact matches. Nearby charts in the closest category:" });
}

function categoriesTool(bundle: Bundle, note: string | null = null) {
  return json(bundle.categories, note);
}
