import { readFileSync } from "node:fs";
import { fileURLToPath } from "node:url";
import { dirname, join } from "node:path";
import { ResourceTemplate } from "@modelcontextprotocol/sdk/server/mcp.js";
import type { McpServer } from "@modelcontextprotocol/sdk/server/mcp.js";
import type { Bundle } from "./types.js";
import { tutorialPayload } from "./tutorial.js";
import { NO_BUNDLE_ERROR } from "./server.js";

const LLMS_TXT_PATH = join(dirname(fileURLToPath(import.meta.url)), "snapshot", "llms.txt");

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
    new ResourceTemplate("bizard://tutorial/{chartId}", { list: undefined }),
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
