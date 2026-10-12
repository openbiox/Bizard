#!/usr/bin/env python3
"""Delete the zh duplicate site_libs/ and images/ trees after refs were
rewritten to the root trees by rewrite-zh-site-refs.py.

Safety: aborts if any zh HTML reference still resolves inside the
duplicate trees (resolution-based, so refs pointing at the root trees
do not trigger a false abort).
"""
import os
import re
import shutil
import sys

site = sys.argv[1] if len(sys.argv) > 1 else "_site"
zh_root = os.path.join(site, "zh")

if not os.path.isdir(zh_root):
    print(f"No zh directory at {zh_root}; nothing to prune")
    sys.exit(0)

# Any relative site_libs/ or images/ reference that resolves INSIDE the zh
# tree means the rewrite step did not cover it — refuse to delete.  The
# prefix alternation mirrors REF_RE in rewrite-zh-site-refs.py (../ and ./).
ref_re = re.compile(r'["\'(=]\s*((?:\.\./|\.\/)*)(?:site_libs|images)/[^"\'()\s>]+')
leftover = 0
for dirpath, _dirnames, filenames in os.walk(zh_root):
    for fname in filenames:
        if not fname.endswith(".html"):
            continue
        fpath = os.path.join(dirpath, fname)
        with open(fpath, encoding="utf-8") as fh:
            content = fh.read()
        for match in ref_re.finditer(content):
            full_ref = match.group(0).lstrip("\"'(= ")
            resolved = os.path.normpath(
                os.path.join(os.path.dirname(fpath), match.group(1) + full_ref)
            )
            if os.path.commonpath([resolved, zh_root]) == zh_root:
                leftover += 1
                print(f"LEFTOVER: {fpath} -> {full_ref}")
                break

if leftover:
    print(f"ERROR: {leftover} zh page(s) still reference the duplicate trees; refusing to delete")
    sys.exit(1)

removed = 0
for tree in ("site_libs", "images"):
    tree_path = os.path.join(zh_root, tree)
    if os.path.isdir(tree_path):
        size_mb = sum(
            os.path.getsize(os.path.join(dp, fn))
            for dp, _, fns in os.walk(tree_path)
            for fn in fns
        ) // (1024 * 1024)
        print(f"Deleting {tree} ({size_mb} MB)")
        shutil.rmtree(tree_path)
        removed += 1

print(f"Pruned {removed} duplicate tree(s) from zh/")
