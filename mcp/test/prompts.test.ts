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
import { describe, expect, it } from "vitest";
import { createBizardServer } from "../src/server.js";
import { validBundle } from "./fixtures.js";
import { clientWith } from "./client.js";

describe("prompt argument completion", () => {
  it("completes the adapt prompt's chartId from the catalog", async () => {
    const client = await clientWith(createBizardServer(validBundle));
    const res = await client.complete({
      ref: { type: "ref/prompt", name: "adapt" },
      argument: { name: "chartId", value: "viol" },
    });
    expect(res.completion.values).toEqual(["ViolinPlot"]);
    await client.close();
  });

  it("completes the troubleshoot prompt's chartId", async () => {
    const client = await clientWith(createBizardServer(validBundle));
    const res = await client.complete({
      ref: { type: "ref/prompt", name: "troubleshoot" },
      argument: { name: "chartId", value: "BOX" },
    });
    expect(res.completion.values).toEqual(["BoxPlot"]);
    await client.close();
  });

  it("yields no completions in degraded mode instead of crashing", async () => {
    const client = await clientWith(createBizardServer(null));
    const res = await client.complete({
      ref: { type: "ref/prompt", name: "adapt" },
      argument: { name: "chartId", value: "viol" },
    });
    expect(res.completion.values).toEqual([]);
    await client.close();
  });
});

describe("prompt message bodies", () => {
  it("recommend embeds the goal and the search workflow", async () => {
    const server = createBizardServer(validBundle);
    const client = await clientWith(server);
    const res = await client.getPrompt({ name: "recommend", arguments: { goal: "show dose response" } });
    expect(res.messages[0].content.text).toContain("show dose response");
    expect(res.messages[0].content.text).toContain("bizard_search");
    await client.close();
  });

  it("troubleshoot embeds the error and the failure taxonomy", async () => {
    const server = createBizardServer(validBundle);
    const client = await clientWith(server);
    const res = await client.getPrompt({
      name: "troubleshoot",
      arguments: { chartId: "ViolinPlot", error: "Error: object 'df' not found" },
    });
    expect(res.messages[0].content.text).toContain("object 'df' not found");
    expect(res.messages[0].content.text).toContain("Classify the failure");
    await client.close();
  });

  it("contribute outlines the check-then-submit flow", async () => {
    const server = createBizardServer(validBundle);
    const client = await clientWith(server);
    const res = await client.getPrompt({ name: "contribute" });
    expect(res.messages[0].content.text).toContain("bizard_contribute_check");
    expect(res.messages[0].content.text).toContain("bizard_contribute_submit");
    await client.close();
  });
});
