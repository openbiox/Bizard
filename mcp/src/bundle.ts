import { readFileSync } from "node:fs";
import { fileURLToPath } from "node:url";
import { dirname, join } from "node:path";
import type { Bundle } from "./types.js";

const SNAPSHOT_PATH = join(dirname(fileURLToPath(import.meta.url)), "snapshot", "bundle.json");

/**
 * Validate and load a bundle. Throws a descriptive error when the input
 * is not a bundle-shaped object — the server falls back to the snapshot
 * (or a startup failure) rather than serving garbage.
 */
export function loadBundle(inline: unknown): Bundle {
  const b = inline as Bundle | undefined;
  if (
    !b || typeof b !== "object" ||
    !Array.isArray(b.charts) || b.charts.length === 0 ||
    !b.charts.every((c) => typeof c?.id === "string" && c.id.length > 0) ||
    !Array.isArray(b.categories) ||
    !b.meta || typeof b.meta !== "object" ||
    !b.citation || typeof b.citation !== "object"
  ) {
    throw new Error("Bundle missing or invalid: expected object with non-empty charts[], categories[], meta, citation");
  }
  return b;
}

/** Read the npm-shipped snapshot; null when absent (fresh checkout without a build). */
export function loadSnapshot(): Bundle | null {
  try {
    return loadBundle(JSON.parse(readFileSync(SNAPSHOT_PATH, "utf-8")));
  } catch {
    return null;
  }
}
