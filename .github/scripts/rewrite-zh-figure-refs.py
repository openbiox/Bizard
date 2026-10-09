#!/usr/bin/env python3
"""Rewrite zh-page figure references to share the English figure trees.

Background: babelquarto renders <name>.zh.qmd under /zh/ but frozen chunk
HTML still references <name>.zh_files/figure-html/... (the path from when
the file carried the .zh suffix). The previous workaround COPIED the whole
English <name>_files/ tree into the zh directory, duplicating every figure
byte in the deployed artifact. Instead, rewrite the references to point at
the already-deployed English figures.

Only references whose English counterpart exists are rewritten; anything
else is left untouched and reported.
"""
import os
import re
import sys

SITE = sys.argv[1] if len(sys.argv) > 1 else "_site"
ZH_ROOT = os.path.join(SITE, "zh")
# Matches a relative reference to a "<name>.zh_files/..." path.
REF_RE = re.compile(r'(?P<prefix>["\'(=])\s*(?P<path>[\w.-]+\.zh_files/[^"\'()\s>]+)')

rewritten = 0
missing = 0
files_touched = 0

for dirpath, _dirnames, filenames in os.walk(ZH_ROOT):
    for fname in filenames:
        if not fname.endswith(".html"):
            continue
        fpath = os.path.join(dirpath, fname)
        with open(fpath, encoding="utf-8") as fh:
            content = fh.read()
        # Directory of the zh page, relative to _site, e.g. "zh" or "zh/Omics"
        rel_dir = os.path.relpath(dirpath, SITE)

        def repl(match):
            global rewritten, missing
            ref = match.group("path")
            name, subpath = ref.split(".zh_files/", 1)
            rel_no_zh = rel_dir[len("zh/"):] if rel_dir != "zh" else ""
            en_target = os.path.normpath(
                os.path.join(SITE, rel_no_zh, name + "_files", subpath)
            )
            if os.path.isfile(en_target):
                # Relative path from the zh page directory to the English file
                fixed = os.path.relpath(en_target, os.path.join(SITE, rel_dir))
                rewritten += 1
                return match.group("prefix") + fixed
            missing += 1
            return match.group(0)

        new_content = REF_RE.sub(repl, content)
        if new_content != content:
            files_touched += 1
            with open(fpath, "w", encoding="utf-8") as fh:
                fh.write(new_content)

print(f"Rewrote {rewritten} reference(s) across {files_touched} zh page(s); {missing} unresolved left as-is")
