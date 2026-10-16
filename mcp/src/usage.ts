import { appendFile, mkdir, readFile } from "node:fs/promises";
import { createHash } from "node:crypto";
import { homedir } from "node:os";
import { dirname, join } from "node:path";

const DEFAULT_LOG = join(homedir(), ".cache", "bizard-mcp", "usage-log.jsonl");

export interface UsageEvent {
  event: "search" | "view" | "adapt" | "cite";
  chartId?: string;
  language?: string;
  query?: string;
}

/** Resolve the active log path: BIZARD_USAGE_LOG wins ("off" disables), else the explicit override, else the default. */
function logPath(override?: string): string {
  const env = process.env.BIZARD_USAGE_LOG;
  if (env !== undefined && env !== "off") return env;
  return override ?? DEFAULT_LOG;
}

function sha256_8(s: string): string {
  return createHash("sha256").update(s).digest("hex").slice(0, 8);
}

/** Append one anonymized usage row (query hashed, never stored verbatim). Returns false when logging is off. */
export async function recordUsage(ev: UsageEvent, overridePath?: string): Promise<boolean> {
  if (process.env.BIZARD_USAGE_LOG === "off") return false;
  const target = logPath(overridePath);
  const row = {
    ts: new Date().toISOString(),
    event: ev.event,
    chartId: ev.chartId ?? null,
    language: ev.language ?? null,
    queryHash: ev.query ? sha256_8(ev.query) : null,
  };
  await mkdir(dirname(target), { recursive: true });
  await appendFile(target, JSON.stringify(row) + "\n", "utf-8");
  return true;
}

/** Read the last `limit` usage rows; a missing file yields []. */
export async function readUsage(limit = 100, overridePath?: string): Promise<Array<Record<string, unknown>>> {
  if (process.env.BIZARD_USAGE_LOG === "off") return [];
  try {
    const txt = await readFile(logPath(overridePath), "utf-8");
    return txt.trim().split("\n").filter(Boolean).slice(-limit)
      .map((l) => JSON.parse(l) as Record<string, unknown>);
  } catch {
    return [];
  }
}
