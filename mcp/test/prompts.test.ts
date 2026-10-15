import { describe, expect, it } from "vitest";
import { createBizardServer } from "../src/server.js";
import { validBundle } from "./fixtures.js";
import { clientWith } from "./client.js";

describe("registered prompts", () => {
  it("exposes recommend / adapt / troubleshoot / contribute", async () => {
    const server = createBizardServer(validBundle);
    const client = await clientWith(server);
    const res = await client.listPrompts();
    expect(res.prompts.map((p) => p.name).sort()).toEqual(["adapt", "contribute", "recommend", "troubleshoot"]);
    await client.close();
  });

  it("adapt prompt embeds chart guidance reference", async () => {
    const server = createBizardServer(validBundle);
    const client = await clientWith(server);
    const res = await client.getPrompt({ name: "adapt", arguments: { chartId: "ViolinPlot" } });
    expect(res.messages[0].content.text).toContain("bizard_adapt_guidance");
    await client.close();
  });
});
