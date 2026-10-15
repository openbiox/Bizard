import { z } from "zod";
import type { McpServer } from "@modelcontextprotocol/sdk/server/mcp.js";
import type { Bundle } from "./types.js";
import { tutorialPayload, codePayload } from "./tutorial.js";
import { jsonResult, errorResult } from "./results.js";
import type { UsageEvent } from "./usage.js";

/** Lazy bundle access so degraded mode (null snapshot) surfaces as a tool error, not a crash. */
export interface LazyBundle {
  get(): Bundle;
}

/** Fire-and-forget usage tracker (local anonymous JSONL, README Privacy). */
export type UsageTracker = (ev: UsageEvent) => void;

export function registerTutorialTools(server: McpServer, bundle: LazyBundle, track: UsageTracker): void {
  server.registerTool("bizard_get_tutorial", {
    title: "Get tutorial structure",
    description: "Structured tutorial content: dependencies, example data URLs, languages available, and tutorial link.",
    inputSchema: { chartId: z.string() },
    annotations: { readOnlyHint: true, openWorldHint: false },
  }, async ({ chartId }) => {
    try {
      const payload = tutorialPayload(bundle.get(), chartId);
      track({ event: "view", chartId });
      return jsonResult(payload);
    } catch (e) {
      return errorResult((e as Error).message);
    }
  });

  server.registerTool("bizard_get_code", {
    title: "Get runnable chart code",
    description: "Runnable code for one chart in R, Python, or Julia with package list and example-data URLs.",
    inputSchema: {
      chartId: z.string(),
      language: z.enum(["r", "python", "julia"]),
    },
    annotations: { readOnlyHint: true, openWorldHint: false },
  }, async ({ chartId, language }) => {
    try {
      const payload = codePayload(bundle.get(), chartId, language);
      track({ event: "view", chartId, language });
      return jsonResult(payload);
    } catch (e) {
      return errorResult((e as Error).message);
    }
  });
}
