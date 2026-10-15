import { z } from "zod";
import type { McpServer } from "@modelcontextprotocol/sdk/server/mcp.js";
import { adaptGuidance } from "./adapt.js";
import { citationPayload } from "./citation.js";
import { jsonResult, errorResult } from "./results.js";
import type { LazyBundle } from "./tutorial-tools.js";

export function registerAdaptTools(server: McpServer, bundle: LazyBundle): void {
  server.registerTool("bizard_adapt_guidance", {
    title: "Get chart adaptation guidance",
    description: "Category-aware checklist for adapting a Bizard chart tutorial to the user's own data: data shape, dependencies, parameter mapping, failure modes, alternatives.",
    inputSchema: {
      chartId: z.string().describe("Chart id from bizard_search"),
      language: z.enum(["r", "python", "julia"]).describe("Target language for the adaptation"),
      userDataContext: z.string().max(2000).optional()
        .describe("Optional description of the user's data/task; echoed into the checklist"),
    },
    annotations: { readOnlyHint: true, openWorldHint: false },
  }, async ({ chartId, language, userDataContext }) => {
    try {
      return jsonResult(adaptGuidance(bundle.get(), chartId, language, userDataContext));
    } catch (e) {
      return errorResult((e as Error).message);
    }
  });

  server.registerTool("bizard_cite", {
    title: "Cite Bizard",
    description: "Citation for the Bizard paper and optionally one chart tutorial: BibTeX, paste-ready methods text, or structured JSON.",
    inputSchema: {
      chartId: z.string().optional().describe("Chart id to cite a specific tutorial (omitted = paper only)"),
      paperOnly: z.boolean().default(false).describe("Cite only the Bizard paper, ignore chartId"),
      format: z.enum(["bibtex", "text", "json"]).default("bibtex").describe("Citation format"),
    },
    annotations: { readOnlyHint: true, openWorldHint: false },
  }, async ({ chartId = null, paperOnly, format }) => {
    try {
      const result = citationPayload(bundle.get(), chartId, paperOnly, format);
      return jsonResult(format === "json" ? result.json : result.text);
    } catch (e) {
      return errorResult((e as Error).message);
    }
  });
}
