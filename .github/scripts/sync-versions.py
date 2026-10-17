#!/usr/bin/env python3
"""Sync every version string in the repo to the released version.

Used by site-release.yml: given a vX.Y.Z site tag, updates DESCRIPTION,
mcp/package.json, mcp/package-lock.json and the MCP server identity so the
whole platform ships under one version line. All edits are computed and
validated in memory first, then written - a mismatch anywhere fails loudly
and leaves the working tree untouched instead of half-synced.

Exit codes: 0 = synced (or already consistent), 2 = skip (the target would
downgrade DESCRIPTION - an @openbiox/bizard-mcp release tag sharing the v*
tag namespace; site-release.yml treats 2 as "do nothing"), 1 = error.
"""

import argparse
import json
import pathlib
import re
import sys

REPO = pathlib.Path(__file__).resolve().parents[2]
SEMVER_RE = re.compile(r"^\d+\.\d+\.\d+$")
DESCRIPTION_VERSION_RE = re.compile(r"(?m)^Version: (\d+\.\d+\.\d+)$")
# Both npm files carry the root package name directly above each root
# "version" field (top level and the packages[""] entry), so the name
# anchors the replacement and a dependency that coincidentally shares the
# old version string can never match.
NPM_ROOT_RE_TMPL = r'("name": "@openbiox/bizard-mcp",\s*"version": )"{version}"'
SERVER_IDENTITY_RE_TMPL = (
    r'(const SERVER_IDENTITY = \{{ name: "bizard-mcp", version: )"{version}"'
)
SERVER_IDENTITY_VERSION_RE = re.compile(
    r'const SERVER_IDENTITY = \{ name: "bizard-mcp", version: "(\d+\.\d+\.\d+)"'
)


def fail(msg: str) -> None:
    print(f"ERROR: {msg}", file=sys.stderr)
    sys.exit(1)


def npm_root_versions(doc: dict, name: str) -> list:
    versions = [doc["version"]]
    if name == "package-lock.json":
        versions.append(doc["packages"][""]["version"])
    return versions


def description_version() -> str:
    path = REPO / "DESCRIPTION"
    match = DESCRIPTION_VERSION_RE.search(path.read_text(encoding="utf-8"))
    if not match:
        fail("DESCRIPTION: no 'Version: X.Y.Z' field found")
    return match.group(1)


def semver_key(version: str) -> tuple:
    return tuple(int(part) for part in version.split("."))


def sync_description(new: str, edits: list) -> None:
    path = REPO / "DESCRIPTION"
    desc = path.read_text(encoding="utf-8")
    current = description_version()
    if current == new:
        edits.append((f"DESCRIPTION: already at {new}", path, None))
        return
    new_text = DESCRIPTION_VERSION_RE.sub(f"Version: {new}", desc, count=1)
    edits.append((f"DESCRIPTION: R package version -> {new}", path, new_text))


def sync_npm(old: str, new: str, edits: list) -> None:
    for name, expected in (("package.json", 1), ("package-lock.json", 2)):
        path = REPO / "mcp" / name
        text = path.read_text(encoding="utf-8")
        doc = json.loads(text)
        current = npm_root_versions(doc, name)
        if all(v == new for v in current):
            edits.append((f"mcp/{name}: already at {new}", path, None))
            continue
        if any(v != old for v in current):
            fail(f"mcp/{name}: versions {current}, expected all {old} or all {new}")
        pattern = re.compile(NPM_ROOT_RE_TMPL.format(version=re.escape(old)))
        new_text, count = pattern.subn(lambda m: m.group(1) + f'"{new}"', text)
        if count != expected:
            fail(f"mcp/{name}: expected {expected} anchored version field(s) "
                 f"at {old}, matched {count}")
        edits.append((f"mcp/{name}: npm version -> {new}", path, new_text))


def sync_server_identity(old: str, new: str, edits: list) -> None:
    path = REPO / "mcp" / "src" / "server.ts"
    ts = path.read_text(encoding="utf-8")
    identity = SERVER_IDENTITY_VERSION_RE.search(ts)
    if not identity:
        fail("mcp/src/server.ts: SERVER_IDENTITY not found")
    if identity.group(1) == new:
        edits.append((f"mcp/src/server.ts: server identity already at {new}",
                      path, None))
        return
    if identity.group(1) != old:
        fail(f"mcp/src/server.ts: identity is {identity.group(1)}, "
             f"expected {old} or {new}")
    pattern = re.compile(SERVER_IDENTITY_RE_TMPL.format(version=re.escape(old)))
    new_text, count = pattern.subn(lambda m: m.group(1) + f'"{new}"', ts)
    if count != 1:
        fail("mcp/src/server.ts: SERVER_IDENTITY pattern not found")
    edits.append((f"mcp/src/server.ts: server identity -> {new}", path, new_text))


def verify(new: str) -> None:
    """Confirm every target now parses and carries the released version."""
    desc = (REPO / "DESCRIPTION").read_text(encoding="utf-8")
    match = DESCRIPTION_VERSION_RE.search(desc)
    if not match or match.group(1) != new:
        fail("DESCRIPTION: post-write version mismatch")
    for name, doc in (("mcp/package.json",
                       json.loads((REPO / "mcp" / "package.json")
                                  .read_text(encoding="utf-8"))),
                      ("mcp/package-lock.json",
                       json.loads((REPO / "mcp" / "package-lock.json")
                                  .read_text(encoding="utf-8")))):
        versions = npm_root_versions(doc, name)
        if any(v != new for v in versions):
            fail(f"{name}: post-write versions {versions}, expected {new}")
    ts = (REPO / "mcp" / "src" / "server.ts").read_text(encoding="utf-8")
    identity = SERVER_IDENTITY_VERSION_RE.search(ts)
    if not identity or identity.group(1) != new:
        fail("mcp/src/server.ts: post-write server identity != " + new)


def main() -> None:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--version", required=True, help="new version as X.Y.Z")
    parser.add_argument("--from", dest="old_version", default=None,
                        help="current version (default: read from mcp/package.json)")
    args = parser.parse_args()

    if not SEMVER_RE.match(args.version):
        fail(f"not a strict X.Y.Z version: {args.version}")
    new = args.version

    # The v* tag namespace is shared with @openbiox/bizard-mcp release tags
    # (v0.1.x-style bot tags). A tag below the current DESCRIPTION version
    # would downgrade the R package, so treat it as an MCP-only release and
    # skip with a distinct exit code instead of erroring.
    if semver_key(new) < semver_key(description_version()):
        print(f"Skip: {new} is below current DESCRIPTION version; "
              "treating as an @openbiox/bizard-mcp release tag.")
        sys.exit(2)

    old = args.old_version or json.loads(
        (REPO / "mcp" / "package.json").read_text(encoding="utf-8"))["version"]

    edits = []
    sync_description(new, edits)
    sync_npm(old, new, edits)
    sync_server_identity(old, new, edits)

    if all(out is None for _line, _path, out in edits):
        # Nothing to change, but still confirm the tree is fully consistent.
        verify(new)
        print(f"All version strings already at {new}; nothing to do.")
        return

    print(f"Syncing versions {old} -> {new}:")
    for line, _path, out in edits:
        print(f"  {line}")
        if out is not None:
            _path.write_text(out, encoding="utf-8")
    verify(new)
    print("done")


if __name__ == "__main__":
    main()
