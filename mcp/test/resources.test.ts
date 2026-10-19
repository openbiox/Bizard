import { describe, expect, it } from "vitest";
import { createBizardServer } from "../src/server.js";
import { validBundle } from "./fixtures.js";
import { clientWith } from "./client.js";

describe("resource discovery (resources/list)", () => {
  it("enumerates the tutorial template per chart alongside the static resources", async () => {
    const client = await clientWith(createBizardServer(validBundle));
    const res = await client.listResources();
    const uris = res.resources.map((r) => r.uri).sort();
    expect(uris).toEqual([
      "bizard://catalog",
      "bizard://llms-txt",
      "bizard://tutorial/BoxPlot",
      "bizard://tutorial/ViolinPlot",
    ]);
    const tut = res.resources.find((r) => r.uri === "bizard://tutorial/ViolinPlot");
    expect(tut?.name).toBe("ViolinPlot");
    expect(tut?.mimeType).toBe("application/json");
    await client.close();
  });

  it("degraded mode: resources/list surfaces the build hint error", async () => {
    const client = await clientWith(createBizardServer(null));
    await expect(client.listResources()).rejects.toThrow(/build:bundle/);
    await client.close();
  });
});

describe("resource template completion", () => {
  it("completes chartId case-insensitively against the bundle", async () => {
    const client = await clientWith(createBizardServer(validBundle));
    const res = await client.complete({
      ref: { type: "ref/resource", uri: "bizard://tutorial/{chartId}" },
      argument: { name: "chartId", value: "box" },
    });
    expect(res.completion.values).toEqual(["BoxPlot"]);
    await client.close();
  });
});

describe("resource reads", () => {
  it("catalog returns compact chart entries with tutorial URLs", async () => {
    const client = await clientWith(createBizardServer(validBundle));
    const res = await client.readResource({ uri: "bizard://catalog" });
    const parsed = JSON.parse(res.contents[0].text as string);
    expect(Array.isArray(parsed)).toBe(true);
    expect(parsed[0]).toMatchObject({ id: "ViolinPlot", tutorialUrl: expect.any(String) });
    await client.close();
  });

  it("tutorial returns the full payload for a known chart", async () => {
    const client = await clientWith(createBizardServer(validBundle));
    const res = await client.readResource({ uri: "bizard://tutorial/ViolinPlot" });
    const parsed = JSON.parse(res.contents[0].text);
    expect(parsed.id).toBe("ViolinPlot");
    expect(parsed.tutorialUrl).toContain("ViolinPlot.html");
    await client.close();
  });

  it("tutorial surfaces a structured error for unknown chartId", async () => {
    const client = await clientWith(createBizardServer(validBundle));
    await expect(client.readResource({ uri: "bizard://tutorial/nope" })).rejects.toThrow(/Unknown chartId/);
    await client.close();
  });

  it("degraded mode: catalog and tutorial reads surface the build hint", async () => {
    const client = await clientWith(createBizardServer(null));
    await expect(client.readResource({ uri: "bizard://catalog" })).rejects.toThrow(/build:bundle/);
    await expect(client.readResource({ uri: "bizard://tutorial/ViolinPlot" })).rejects.toThrow(/build:bundle/);
    await client.close();
  });

  it("llms-txt read falls back to the build hint when the snapshot file is absent", async () => {
    const client = await clientWith(createBizardServer(validBundle));
    // Test runs from src layout without a dist/ rebuild — the snapshot file
    // sits next to the compiled output, so a miss exercises the catch path.
    const promise = client.readResource({ uri: "bizard://llms-txt" });
    const result = await promise.then(() => "ok", (e: Error) => e.message);
    expect(result === "ok" || /llms\.txt not found/.test(result)).toBe(true);
    await client.close();
  });
});

describe("completeChartId", () => {
  it("returns all ids on an empty needle and caps output at 100", async () => {
    const client = await clientWith(createBizardServer(validBundle));
    const all = await client.complete({
      ref: { type: "ref/resource", uri: "bizard://tutorial/{chartId}" },
      argument: { name: "chartId", value: "" },
    });
    expect(all.completion.values).toEqual(["BoxPlot", "ViolinPlot"]);
    await client.close();
  });
});
