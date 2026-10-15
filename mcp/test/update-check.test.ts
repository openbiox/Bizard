import { describe, expect, it, vi } from "vitest";
import { checkForUpdate, createNoticeQueue, updateNotice, REMOTE_BUNDLE_URL } from "../src/update-check.js";
import { validBundle } from "./fixtures.js";
import type { Bundle } from "../src/types.js";

function fetchOf(headEtag: string | null, body: unknown, log: string[] = []): typeof fetch {
  return (async (url: string | URL, init?: RequestInit) => {
    if (init?.method === "HEAD") {
      log.push(`HEAD ${url}`);
      return new Response(null, { status: 200, headers: headEtag ? { ETag: headEtag } : {} });
    }
    log.push(`GET ${url}`);
    return new Response(JSON.stringify(body), {
      status: 200,
      headers: headEtag ? { ETag: headEtag } : {},
    });
  }) as typeof fetch;
}

const twoChartBundle = (): Bundle => ({
  ...validBundle,
  meta: { ...validBundle.meta, bundleVersion: 2 },
  charts: [...validBundle.charts, { ...validBundle.charts[0], id: "PiePlot-1", name: "Pie Chart" }],
});

describe("checkForUpdate", () => {
  it("HEADs the remote URL, swaps on ETag change, queues one-time notice with new/updated counts", async () => {
    const log: string[] = [];
    const doFetch = fetchOf('"remote-2"', twoChartBundle(), log);
    const holder = { bundle: validBundle, etag: '"remote-1"' as string | null };
    const swap = vi.fn();
    const notice = createNoticeQueue();

    const result = await checkForUpdate(holder, { fetch: doFetch, onSwap: swap, notices: notice });

    expect(result).toBe(true);
    expect(log).toEqual([`HEAD ${REMOTE_BUNDLE_URL}`, `GET ${REMOTE_BUNDLE_URL}`]);
    expect(swap).toHaveBeenCalledTimes(1);
    const next = updateNotice(notice);
    expect(next).toMatch(/内容包已更新至 v2/);
    expect(next).toMatch(/新增 1 图|更新 \d+ 图/);
    // one-shot: queue drained
    expect(updateNotice(notice)).toBeNull();
  });

  it("does nothing when the remote ETag is unchanged", async () => {
    const log: string[] = [];
    const doFetch = fetchOf('"remote-1"', twoChartBundle(), log);
    const holder = { bundle: validBundle, etag: '"remote-1"' as string | null };
    const swap = vi.fn();

    const result = await checkForUpdate(holder, { fetch: doFetch, onSwap: swap, notices: createNoticeQueue() });

    expect(result).toBe(false);
    expect(log).toEqual([`HEAD ${REMOTE_BUNDLE_URL}`]);
    expect(swap).not.toHaveBeenCalled();
  });

  it("falls back silently when the network fails (no swap, no throw)", async () => {
    const doFetch = (async () => { throw new Error("offline"); }) as typeof fetch;
    const swap = vi.fn();
    const stderr = vi.fn();

    const result = await checkForUpdate(
      { bundle: validBundle, etag: null },
      { fetch: doFetch, onSwap: swap, notices: createNoticeQueue(), stderr },
    );

    expect(result).toBe(false);
    expect(swap).not.toHaveBeenCalled();
    expect(stderr).toHaveBeenCalledTimes(1);
  });

  it("falls back silently when the remote payload fails loadBundle validation", async () => {
    const malformed = { charts: "nope" };
    const doFetch = fetchOf('"remote-9"', malformed);
    const swap = vi.fn();

    const result = await checkForUpdate(
      { bundle: validBundle, etag: null },
      { fetch: doFetch, onSwap: swap, notices: createNoticeQueue() },
    );

    expect(result).toBe(false);
    expect(swap).not.toHaveBeenCalled();
  });

  it("treats a HEAD without ETag as no-update (cannot compare)", async () => {
    const log: string[] = [];
    const doFetch = fetchOf(null, twoChartBundle(), log);
    const swap = vi.fn();

    const result = await checkForUpdate(
      { bundle: validBundle, etag: null },
      { fetch: doFetch, onSwap: swap, notices: createNoticeQueue() },
    );

    expect(result).toBe(false);
    expect(log).toEqual([`HEAD ${REMOTE_BUNDLE_URL}`]);
    expect(swap).not.toHaveBeenCalled();
  });
});

describe("createNoticeQueue / updateNotice", () => {
  it("returns null when no update has been queued", () => {
    expect(updateNotice(createNoticeQueue())).toBeNull();
  });

  it("returns each queued notice exactly once", () => {
    const notices = createNoticeQueue();
    notices.push("notice-a", "notice-b");
    expect(updateNotice(notices)).toBe("notice-a");
    expect(updateNotice(notices)).toBe("notice-b");
    expect(updateNotice(notices)).toBeNull();
  });
});
