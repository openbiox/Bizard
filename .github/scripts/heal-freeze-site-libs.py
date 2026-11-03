#!/usr/bin/env python3
"""Drop freeze entries whose recorded site_libs references are missing.

Frozen pages record their site_libs references in
execute-results/html.json, and babelquarto copies the dependency files
from _freeze/site_libs during the frozen render. A page whose references
are missing from _freeze/site_libs (e.g. written before a repinned R
package started emitting a new dependency version) renders with dangling
asset links; deleting its freeze entry makes the pre-render re-execute
the page and repopulate both. Julia pages are skipped: the pre-render
work list never covers them, so a dropped entry could not be repopulated.
"""
import glob
import os
import re
import shutil
import sys

freeze = "_freeze"
site_libs = os.path.join(freeze, "site_libs")
if not os.path.isdir(freeze) or not os.path.isdir(site_libs):
    print("No freeze cache or site_libs; nothing to heal")
    sys.exit(0)

# Mirrors REF_RE in rewrite-zh-site-refs.py: only references preceded by
# a quote/paren/equals are asset URLs, and ./ or ../ page-relative
# prefixes must be handled (nested pages emit ../site_libs/...).
ref_re = re.compile(
    r'(?<=[\"\'(=])(?:\.\./|\.\/)*(site_libs/[^\\\"\'()\s>]+)'
)

dropped = []
julia_broken = 0
for meta in glob.glob(
    os.path.join(freeze, "**", "execute-results", "html.json"), recursive=True
):
    page_dir = os.path.dirname(os.path.dirname(meta))
    if os.path.samefile(page_dir, freeze):
        continue
    with open(meta, encoding="utf-8", errors="ignore") as fh:
        content = fh.read()
    missing = set()
    for match in ref_re.finditer(content):
        rel = match.group(1)[len("site_libs/"):]
        if ".." in rel.split("/"):
            continue
        if not os.path.exists(os.path.join(site_libs, rel)):
            missing.add(rel)
    if not missing:
        continue
    if os.path.basename(os.path.dirname(page_dir)) == "Julia":
        # The pre-render work list never covers Julia pages, so a dropped
        # entry could not be repopulated here. Report instead of healing.
        julia_broken += 1
        print(
            f"  (skipped) {os.path.relpath(page_dir, freeze)} -> "
            f"{sorted(missing)[0]}"
        )
        continue
    shutil.rmtree(page_dir)
    dropped.append(f"{os.path.relpath(page_dir, freeze)} -> {sorted(missing)[0]}")

if julia_broken:
    print(f"Self-heal: {julia_broken} stale Julia freeze page(s) left in place")
print(f"Self-heal: dropped {len(dropped)} stale freeze page(s)")
for entry in dropped:
    print(f"  {entry}")
if len(dropped) > 25:
    print(
        f"::warning::{len(dropped)} freeze page(s) referenced missing site_libs "
        "assets and were re-queued for a full render"
    )
