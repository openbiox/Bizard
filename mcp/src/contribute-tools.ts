import { z } from "zod";
import type { McpServer } from "@modelcontextprotocol/sdk/server/mcp.js";
import { checkContribution, submitContribution } from "./contribute.js";
import { jsonResult, errorResult } from "./results.js";

const USER_CONTEXT_MAX_CHARS = 2000;

export function registerContributeTools(server: McpServer): void {
  server.registerTool("bizard_contribute_check", {
    title: "Check a contribution draft",
    description: "Static checks on a draft tutorial/fix before submitting to Bizard: front matter, Setup section, executable code blocks; previews contribution points.",
    inputSchema: {
      contributionType: z.enum(["new-tutorial", "fix", "translation", "gallery-image"]),
      content: z.string().max(200_000).describe("Full draft content (markdown/QMD) to check"),
      language: z.enum(["en", "zh"]),
    },
    annotations: { readOnlyHint: true, openWorldHint: false },
  }, async ({ contributionType, content, language }) => {
    try {
      return jsonResult(checkContribution({ contributionType, content, language }));
    } catch (e) {
      return errorResult((e as Error).message);
    }
  });

  server.registerTool("bizard_contribute_submit", {
    title: "Submit a contribution to Bizard",
    description: "Submit a contribution as a GitHub issue. Degradation chain: gh CLI → GITHUB_TOKEN API → draft + deep-link for manual browser submission.",
    inputSchema: {
      contributionType: z.enum(["new-tutorial", "fix", "translation", "gallery-image"]),
      title: z.string().max(200).describe("Issue title"),
      body: z.string().max(100_000).describe("Issue body (markdown); over-long bodies are truncated"),
      asPR: z.boolean().default(false).describe("Kept for future PR submission; issues only today"),
    },
    annotations: { readOnlyHint: false, openWorldHint: true },
  }, async ({ contributionType, title, body, asPR }) => {
    try {
      return jsonResult(await submitContribution({ contributionType, title, body, asPR }, {}));
    } catch (e) {
      return errorResult((e as Error).message);
    }
  });
}
