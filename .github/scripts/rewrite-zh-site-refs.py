#!/usr/bin/env python3
"""Rewrite zh-page asset references to share the root site trees.

Background: babelquarto renders the zh pass into _site/zh/ and copies the
shared site_libs/ and images/ trees into _site/zh/ as byte-identical
duplicates of the root copies (_site/site_libs/, _site/images/), roughly
94 MB per deploy.  This script rewrites every zh HTML reference that
resolves under zh/site_libs/ or zh/images/ to the root-tree equivalent,
so both zh duplicate trees can be deleted afterwards.

Only references whose root-tree target exists are rewritten; anything
else is left untouched and reported.  Run BEFORE deleting the duplicate
trees (deletion is done by prune-zh-duplicates.sh).
"""
import os
import re
import sys

SITE = sys.argv[1] if len(sys.argv) > 1 else "_site"
ZH_ROOT = os.path.join(SITE, "zh")
# Matches a relative reference whose first segment is site_libs/ or images/
# (optionally preceded by ../ or ./).  Quarto emits page-relative asset URLs:
# root-level pages (index, About, ...) emit "./images/...", category pages
# emit "../images/...", so both prefixes must be handled — a "./"-prefixed
# ref that goes unmatched here would silently survive and 404 after the
# duplicate zh tree is pruned.  Absolute, scheme-relative and srcset-less
# attributes only; url(...) inside CSS also covered via the ( prefix.
REF_RE = re.compile(
    r'(?P<prefix>["\'(=])\s*(?P<dots>(?:\.\./|\.\/)*)?(?P<path>(?:site_libs|images)/[^"\'()\s>]+)'
)

rewritten = 0
missing = 0
files_touched = 0


def repl(match, rel_dir):
    global rewritten, missing
    dots = match.group("dots") or ""
    path = match.group("path")
    # Resolve the reference against the page directory (zh-relative).
    zh_target = os.path.normpath(os.path.join(SITE, rel_dir, dots + path))
    rel_to_zh_root = os.path.relpath(zh_target, ZH_ROOT)
    if rel_to_zh_root.startswith(os.pardir):
        missing += 1
        return match.group(0)
    root_target = os.path.join(SITE, rel_to_zh_root)
    if not os.path.isfile(root_target):
        missing += 1
        return match.group(0)
    fixed = os.path.relpath(root_target, os.path.join(SITE, rel_dir))
    rewritten += 1
    return match.group("prefix") + fixed


for dirpath, _dirnames, filenames in os.walk(ZH_ROOT):
    for fname in filenames:
        if not fname.endswith(".html"):
            continue
        fpath = os.path.join(dirpath, fname)
        with open(fpath, encoding="utf-8") as fh:
            content = fh.read()
        # Directory of the zh page, relative to _site, e.g. "zh" or "zh/Omics"
        rel_dir = os.path.relpath(dirpath, SITE)

        def sub(match):
            return repl(match, rel_dir)

        new_content = REF_RE.sub(sub, content)
        if new_content != content:
            files_touched += 1
            with open(fpath, "w", encoding="utf-8") as fh:
                fh.write(new_content)

print(
    f"Rewrote {rewritten} reference(s) across {files_touched} zh page(s); "
    f"{missing} unresolved left as-is"
)
