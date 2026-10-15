import { describe, expect, it, vi } from "vitest";
import { runCli } from "../src/entry.js";
import { validBundle } from "./fixtures.js";

describe("runCli", () => {
  it("starts degraded when the snapshot is missing: server connects, stderr notes the build hint", async () => {
    const stderr = vi.fn();
    const connect = vi.fn().mockResolvedValue(undefined);
    await runCli([], { loadSnapshotFn: () => null, connect, stderr });
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
    });
    expect(startHttp).toHaveBeenCalledTimes(1);
    expect(startHttp).toHaveBeenCalledWith(expect.anything(), 4599);
  });

  it("uses the default HTTP port when BIZARD_HTTP_PORT is unset", async () => {
    const startHttp = vi.fn();
    await runCli(["--http"], { loadSnapshotFn: () => validBundle, startHttp, env: {} });
    expect(startHttp).toHaveBeenCalledWith(expect.anything(), 3000);
  });

  it("starts silently in normal stdio mode", async () => {
    const stderr = vi.fn();
    const connect = vi.fn().mockResolvedValue(undefined);
    await runCli([], { loadSnapshotFn: () => validBundle, connect, stderr });
    expect(connect).toHaveBeenCalledTimes(1);
    expect(stderr).not.toHaveBeenCalled();
  });
});
