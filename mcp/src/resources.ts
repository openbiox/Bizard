import { readFileSync } from "node:fs";
import { fileURLToPath } from "node:url";
import { dirname, join } from "node:path";
import { ResourceTemplate } from "@modelcontextprotocol/sdk/server/mcp.js";
import type { McpServer } from "@modelcontextprotocol/sdk/server/mcp.js";
import type { Bundle } from "./types.js";
import { tutorialPayload } from "./tutorial.js";
import { NO_BUNDLE_ERROR } from "./server.js";

const LLMS_TXT_PATH = join(dirname(fileURLToPath(import.meta.url)), "snapshot", "llms.txt");
/** Spec caps completion values at 100; keep the payload small and predictable. */
const COMPLETION_LIMIT = 100;

/** Chart ids for completion/list, lowercase-filtered against what the user typed. */
export function completeChartId(bundle: Bundle, value: string): string[] {
  const needle = value.toLowerCase();
  return bundle.charts
    .map((c) => c.id)
    .filter((id) => needle === "" || id.toLowerCase().includes(needle))
    .sort((a, b) => a.localeCompare(b))
    .slice(0, COMPLETION_LIMIT);
}

function textResult(text: string) {
  return { contents: [{ uri: "", text }] };
}

export function registerResources(server: McpServer, getBundle: () => Bundle | null): void {
  server.registerResource(
    "catalog",
    "bizard://catalog",
    { title: "Chart catalog", description: "Compact JSON of all Bizard charts (id, name, category, URLs).", mimeType: "application/json" },
    () => {
      const bundle = getBundle();
      if (!bundle) throw new Error(NO_BUNDLE_ERROR);
      const compact = bundle.charts.map((c) => ({
        id: c.id, name: c.name, level1: c.level1, level2: c.level2, tutorialUrl: c.tutorialUrl,
      }));
      return textResult(JSON.stringify(compact, null, 2));
    },
  );

  server.registerResource(
    "tutorial",
    new ResourceTemplate("bizard://tutorial/{chartId}", {
      // Enumerate per-chart URIs so clients can discover tutorials without a
      // tool round-trip; the per-item name/title override the template metadata.
      list: () => {
        const bundle = getBundle();
        if (!bundle) throw new Error(NO_BUNDLE_ERROR);
        return {
          resources: bundle.charts.map((c) => ({
            uri: `bizard://tutorial/${c.id}`,
            name: c.id,
            title: c.name,
            description: c.description,
            mimeType: "application/json",
          })),
        };
      },
      complete: {
        chartId: (value) => {
          const bundle = getBundle();
          if (!bundle) return [];
          return completeChartId(bundle, value);
        },
      },
    }),
    { title: "Tutorial payload", description: "Full tutorial payload for one chart.", mimeType: "application/json" },
    (uri, variables) => {
      const bundle = getBundle();
      if (!bundle) throw new Error(NO_BUNDLE_ERROR);
      const payload = tutorialPayload(bundle, String(variables.chartId));
      return { contents: [{ uri: uri.href, text: JSON.stringify(payload, null, 2), mimeType: "application/json" }] };
    },
  );

  server.registerResource(
    "llms-txt",
    "bizard://llms-txt",
    { title: "llms.txt", description: "Bizard llms.txt overview (present only in built snapshots).", mimeType: "text/plain" },
    () => {
      try {
        return textResult(readFileSync(LLMS_TXT_PATH, "utf-8"));
      } catch {
        throw new Error("bizard-mcp: llms.txt not found — run `npm run build:bundle` to generate the snapshot, then rebuild.");
      }
    },
  );
}
