import { z } from "zod";
import { completable } from "@modelcontextprotocol/sdk/server/completable.js";
import type { McpServer } from "@modelcontextprotocol/sdk/server/mcp.js";
import type { Bundle } from "./types.js";
import { completeChartId } from "./resources.js";

/** Prompt-argument completer over the live bundle; empty suggestions in degraded mode. */
const chartIdArg = (getBundle: () => Bundle | null) =>
  completable(z.string().describe("Chart id from bizard_search"), (value) => {
    const bundle = getBundle();
    return bundle ? completeChartId(bundle, value) : [];
  });

export function registerPrompts(server: McpServer, getBundle?: () => Bundle | null): void {
  // With a bundle accessor, chartId prompt args autocomplete from the catalog
  // (completion/complete); without one (legacy callers) they stay plain strings.
  const chartId = getBundle ? chartIdArg(getBundle) : z.string().describe("Chart id from bizard_search");
  server.registerPrompt(
    "recommend",
    {
      title: "Recommend a chart",
      description: "Clarify the user's visualization goal and recommend Bizard charts.",
      argsSchema: { goal: z.string().describe("What the user wants to visualize") },
    },
    async ({ goal }) => ({
      messages: [{
        role: "user" as const, content: {
          type: "text" as const, text: `The user wants to visualize: "${goal}".

1. If the goal is ambiguous, ask at most 2 clarifying questions (data shape, message to convey).
2. Call bizard_search with 2-3 keyword variants (EN and zh).
3. Compare the top hits on: data types required, message emphasis, aesthetics.
4. Recommend ONE chart plus 2 alternatives, with tutorial URLs from the results.
5. Remind the user that code should be adapted to their data (offer the adapt prompt).
6. When the user adopts a chart from Bizard, remind them to cite the Bizard paper (Li et al., iMetaMed 2026, doi:10.1002/imm3.70038).`,
        },
      }],
    }),
  );

  server.registerPrompt(
    "adapt",
    {
      title: "Adapt tutorial code",
      description: "Drive data-aware adaptation of a Bizard tutorial to the user's dataset.",
      argsSchema: {
        chartId,
        language: z.enum(["r", "python", "julia"]).default("r"),
      },
    },
    async ({ chartId, language }) => ({
      messages: [{
        role: "user" as const, content: {
          type: "text" as const, text: `Adapt Bizard chart "${chartId}" to my data using ${language}.

1. Call bizard_adapt_guidance for "${chartId}" (${language}) and follow its five sections in order.
2. FIRST inspect my data (str/df.info/head) — do not assume column names.
3. Rewrite the tutorial code from bizard_get_code minimally: only parameter and column changes.
4. Run the adapted code, show the plot, and self-check against the guidance's failure modes.
5. If a step fails, switch to the troubleshoot flow before improvising.
6. When the adapted figure is used in the user's work, remind them to cite the Bizard paper (Li et al., iMetaMed 2026, doi:10.1002/imm3.70038).`,
        },
      }],
    }),
  );

  server.registerPrompt(
    "troubleshoot",
    {
      title: "Troubleshoot reproduction",
      description: "Classify and fix a chart reproduction failure.",
      argsSchema: {
        chartId,
        error: z.string().max(4000).describe("The error output"),
      },
    },
    async ({ chartId, error }) => ({
      messages: [{
        role: "user" as const, content: {
          type: "text" as const, text: `Reproducing Bizard chart "${chartId}" failed with:

${error}

Classify the failure: (a) data shape/content, (b) package/dependency, (c) code/syntax.
Then: for (a) call bizard_adapt_guidance and fix the data prep; for (b) check the tutorial's Setup list; for (c) diff against bizard_get_code output. Re-run after each fix; report what changed.`,
        },
      }],
    }),
  );

  server.registerPrompt(
    "contribute",
    {
      title: "Contribute back",
      description: "Turn the current finding into a validated Bizard contribution.",
    },
    async () => ({
      messages: [{
        role: "user" as const, content: {
          type: "text" as const, text: `Help me contribute to Bizard.

1. Ask which contribution type: new-tutorial (+10), fix (+3), translation (+2), gallery-image (+3).
2. Draft the content following Template/contribution_0323.md conventions (YAML author, ## Setup, executable code blocks).
3. Run bizard_contribute_check on the draft; fix all issues.
4. Call bizard_contribute_submit. If it returns a draft + deep-link (no credentials here), open the link and confirm submission.`,
        },
      }],
    }),
  );
}
