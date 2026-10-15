import { describe, expect, it, vi } from "vitest";
import { runCli } from "../src/entry.js";
import { connect } from "./client.js";
import { validBundle } from "./fixtures.js";

const NO_CHECK = { startUpdateCheck: vi.fn() };

describe("runCli", () => {
  it("starts degraded when the snapshot is missing: server connects, stderr notes the build hint", async () => {
    const stderr = vi.fn();
    const connect = vi.fn().mockResolvedValue(undefined);
    await runCli([], { loadSnapshotFn: () => null, connect, stderr, ...NO_CHECK });
    expect(connect).toHaveBeenCalledTimes(1);
    expect(stderr).toHaveBeenCalledTimes(1);
    expect(stderr).toHaveBeenCalledWith(expect.stringMatching(/npm run build:bundle/));
  });

  it("routes --http to the HTTP server honoring BIZARD_HTTP_PORT", async () => {
    const startHttp = vi.fn();
    await runCli(["--http"], {
      loadSnapshotFn: () => validBundle,
      startHttp,
      env: { BIZARD_HTTP_PORT: "4599" },
      ...NO_CHECK,
    });
    expect(startHttp).toHaveBeenCalledTimes(1);
    expect(startHttp).toHaveBeenCalledWith(expect.anything(), 4599);
  });

  it("uses the default HTTP port when BIZARD_HTTP_PORT is unset", async () => {
    const startHttp = vi.fn();
    await runCli(["--http"], { loadSnapshotFn: () => validBundle, startHttp, env: {}, ...NO_CHECK });
    expect(startHttp).toHaveBeenCalledWith(expect.anything(), 3000);
  });

  it("starts silently in normal stdio mode", async () => {
    const stderr = vi.fn();
    const connect = vi.fn().mockResolvedValue(undefined);
    await runCli([], { loadSnapshotFn: () => validBundle, connect, stderr, ...NO_CHECK });
    expect(connect).toHaveBeenCalledTimes(1);
    expect(stderr).not.toHaveBeenCalled();
  });

  it("kicks off the spec §3.4 update check with the loaded bundle and a fresh notice queue", async () => {
    const startUpdateCheck = vi.fn();
    const connect = vi.fn().mockResolvedValue(undefined);
    await runCli([], { loadSnapshotFn: () => validBundle, connect, startUpdateCheck });

    expect(startUpdateCheck).toHaveBeenCalledTimes(1);
    const [holder, notices] = startUpdateCheck.mock.calls[0];
    expect(holder.bundle).toBe(validBundle);
    expect(holder.etag).toBeNull();
    expect(notices).toEqual([]);
  });

  it("hands the server a live holder view: a mid-session swap reaches the next tool result", async () => {
    let observed: string | undefined;
    await runCli([], {
      loadSnapshotFn: () => validBundle,
      stderr: () => {},
      startUpdateCheck: (holder, notices) => {
        holder.bundle = {
          ...validBundle,
          charts: [...validBundle.charts, { ...validBundle.charts[0], id: "PiePlot-1", name: "Pie Chart" }],
        };
        notices.push("[Bizard 内容包已更新至 v2:新增 1 图、更新 0 图]");
      },
      connect: async (server) => {
        const client = await connect(server);
        const res = await client.callTool({ name: "bizard_list_categories", arguments: {} });
        observed = JSON.parse((res.content as Array<{ type: string; text: string }>)[0].text).note;
        await client.close();
      },
    });
    expect(observed).toBe("[Bizard 内容包已更新至 v2:新增 1 图、更新 0 图]");
  });
});
