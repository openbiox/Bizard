import { loadBundle } from "./bundle.js";
import type { Bundle } from "./types.js";

/** CI publishes the bundle here (mcp-bundle.yml → files/mcp/bundle.json on main). */
export const REMOTE_BUNDLE_URL = "https://openbiox.github.io/Bizard/files/mcp/bundle.json";

/** Mutable cell the server reads per-call; the update check hot-swaps its contents. */
export interface MutableHolder {
  bundle: Bundle | null;
  etag: string | null;
}

export type NoticeQueue = string[];

export interface UpdateCheckDeps {
  fetch?: typeof fetch;
  onSwap?: (bundle: Bundle, etag: string) => void;
  notices: NoticeQueue;
  stderr?: (line: string) => void;
}

function diffCounts(oldB: Bundle | null, newB: Bundle): { added: number; updated: number } {
  if (!oldB) return { added: newB.charts.length, updated: 0 };
  const oldIds = new Set(oldB.charts.map((c) => c.id));
  let shared = 0;
  for (const c of newB.charts) if (oldIds.has(c.id)) shared += 1;
  return { added: newB.charts.length - shared, updated: shared };
}

export function createNoticeQueue(): NoticeQueue {
  return [];
}

export function updateNotice(notices: NoticeQueue): string | null {
  return notices.shift() ?? null;
}

/**
 * Spec §3.4: background HEAD of the remote bundle comparing ETag; on change, GET +
 * validate + hot-swap, queueing a ONE-TIME notice for the session's next tool result.
 * Offline / network / validation failure: silent fallback to the in-memory snapshot,
 * one stderr line, never throws.
 */
export async function checkForUpdate(holder: MutableHolder, deps: UpdateCheckDeps): Promise<boolean> {
  const doFetch = deps.fetch ?? globalThis.fetch;
  const stderr = deps.stderr ?? ((line: string) => console.error(line));
  try {
    const head = await doFetch(REMOTE_BUNDLE_URL, { method: "HEAD" });
    const etag = head.headers.get("ETag");
    if (!etag || etag === holder.etag) return false;

    const res = await doFetch(REMOTE_BUNDLE_URL);
    if (!res.ok) return false;
    const remote = loadBundle(await res.json());
    const { added, updated } = diffCounts(holder.bundle, remote);
    holder.bundle = remote;
    holder.etag = etag;
    deps.onSwap?.(remote, etag);
    deps.notices.push(`[Bizard 内容包已更新至 v${remote.meta.bundleVersion}:新增 ${added} 图、更新 ${updated} 图]`);
    return true;
  } catch (e) {
    stderr(`bizard-mcp: update check failed, using built-in snapshot (${(e as Error).message})`);
    return false;
  }
}
