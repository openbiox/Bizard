import { describe, expect, it } from "vitest";
import { connect } from "./client.js";
import { createBizardServer } from "../src/server.js";
import { validBundle } from "./fixtures.js";
import type { NoticeQueue } from "../src/update-check.js";

describe("update-notice plumbing", () => {
  it("prepends a queued one-time notice to the next tool result, then stops", async () => {
    const notices: NoticeQueue = ["[Bizard 内容包已更新至 v2:新增 1 图、更新 0 图]"];
    const server = createBizardServer({ get: () => validBundle }, notices);
    const client = await connect(server);

    const first = await client.callTool({ name: "bizard_list_categories", arguments: {} });
    expect(JSON.parse((first.content as Array<{ type: string; text: string }>)[0].text).note)
      .toBe("[Bizard 内容包已更新至 v2:新增 1 图、更新 0 图]");

    const second = await client.callTool({ name: "bizard_list_categories", arguments: {} });
    expect(JSON.parse((second.content as Array<{ type: string; text: string }>)[0].text).note).toBeUndefined();
    await client.close();
  });

  it("leaves results untouched when no notice is queued", async () => {
    const server = createBizardServer({ get: () => validBundle }, []);
    const client = await connect(server);
    const res = await client.callTool({ name: "bizard_list_categories", arguments: {} });
    expect(JSON.parse((res.content as Array<{ type: string; text: string }>)[0].text).note).toBeUndefined();
    await client.close();
  });
});
