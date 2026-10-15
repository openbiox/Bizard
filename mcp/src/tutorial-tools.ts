import { z } from "zod";
import type { McpServer } from "@modelcontextprotocol/sdk/server/mcp.js";
import type { Bundle } from "./types.js";
import { tutorialPayload, codePayload } from "./tutorial.js";
import { jsonResult, errorResult } from "./results.js";

/** Lazy bundle access so degraded mode (null snapshot) surfaces as a tool error, not a crash. */
export interface LazyBundle {
  get(): Bundle;
}

export function registerTutorialTools(server: McpServer, bundle: LazyBundle): void {
  server.registerTool("bizard_get_tutorial", {
    title: "Get tutorial structure",
    description: "Structured tutorial content: dependencies, example data URLs, languages available, and tutorial link.",
    inputSchema: { chartId: z.string() },
    annotations: { readOnlyHint: true, openWorldHint: false },
  }, async ({ chartId }) => {
    try {
      return jsonResult(tutorialPayload(bundle.get(), chartId));
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
      return jsonResult(codePayload(bundle.get(), chartId, language));
    } catch (e) {
      return errorResult((e as Error).message);
    }
  });
}
