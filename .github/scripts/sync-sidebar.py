#!/usr/bin/env python3
"""Synchronize the `_quarto.yml` sidebar `contents` list with the chart QMDs on disk.

Issue #306, C3: adding a chart page today means hand-editing a ~570-line
sidebar block. This script makes "add one qmd" the only required step:

- The existing `contents` block stays the single source of truth for
  order, display text, icons, and manual entries. It is never reordered;
  the script only APPENDS entries for pages missing from the sidebar.
- A missing page is appended to the unique sidebar section that already
  links other pages from the same directory, using the qmd frontmatter
  `title` as display text.
- Chinese (`.zh.qmd`) files are skipped: babelquarto renders them from
  their English pairs and the sidebar never links them.
- Files listed as commented-out entries (e.g. the under-construction
  `Proportion/EdgeBundling.qmd`) keep their excluded status: a commented
  href counts as "known", so the generator never re-adds them.
- Files under an explicit exclusion path (see EXCLUDED_PATHS) are never
  added even if uncommented later.

Placement is derived from the sidebar itself, never guessed. The script
reports (and, in `--check`, fails on) pages it cannot place safely:

- `Hiplot/` pages span four sidebar sections (BASICS/OMICS/CLINICS/MAP)
  with interleaved file-number ranges, so new Hiplot pages always need
  manual placement.
- So do pages without a frontmatter `title`, and pages in a directory no
  section links yet. Adding a brand-new category means adding its
  directory to CHART_DIRS below plus a manual sidebar section.

Modes:
    python3 .github/scripts/sync-sidebar.py --check    # exit 1 if out of sync (CI)
    python3 .github/scripts/sync-sidebar.py --update   # rewrite _quarto.yml in place
"""
import argparse
import os
import re
import sys

DEFAULT_ROOT = os.path.dirname(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))

# Chart directories scanned for English chart pages.
CHART_DIRS = [
    "Distribution",
    "Correlation",
    "Ranking",
    "Composition",
    "DataOverTime",
    "Proportion",
    "Animation",
    "Omics",
    "Python",
    "Julia",
    "Clinics",
    "Hiplot",
]

# Chart pages kept out of the navigation on purpose. A commented-out
# sidebar entry (e.g. Proportion/EdgeBundling.qmd) already counts as
# "known"; list a path here when it must stay excluded even if its sidebar
# comment is ever deleted.
EXCLUDED_PATHS = {
    "Proportion/EdgeBundling.qmd",  # under construction, commented out in sidebar
}

SECTION_RE = re.compile(r"^      - section:")
TOP_LEVEL_ITEM_RE = re.compile(r"^      - text:")
HREF_RE = re.compile(r"href:\s*\"?(\S+\.qmd)\"?")
CONTENTS_BLOCK_RE = re.compile(r"^    contents:\n(.*?)^format:", re.M | re.S)

# Indentation of appended entries, matching the existing section contents.
ENTRY_INDENT = " " * 10
HREF_INDENT = " " * 12


def parse_sidebar(text):
    """Parse the `website.sidebar.contents` block of _quarto.yml.

    Returns (lines, sections, linked, known) where `lines` is the file
    split on newlines, `sections` lists the `- section:` blocks in
    document order (each with its `hrefs` and the line index of its last
    active entry, `insert_after`), `linked` holds every active href in
    the block, and `known` additionally includes commented-out hrefs.
    """
    block = CONTENTS_BLOCK_RE.search(text)
    if block is None:
        sys.exit("error: could not locate the website.sidebar.contents block in _quarto.yml")
    eol = "\r\n" if "\r\n" in text else "\n"
    lines = text.split(eol)
    start = text[: block.start(1)].count("\n")
    end = text[: block.end(1)].count("\n")  # line index of the `format:` key

    sections = []
    linked = set()
    known = set()
    current = None
    for i in range(start, end):
        line = lines[i]
        if SECTION_RE.match(line):
            current = {"hrefs": [], "insert_after": None}
            sections.append(current)
            continue
        if TOP_LEVEL_ITEM_RE.match(line):
            current = None  # trailing top-level links (Tool Links, About Us)
        href = HREF_RE.search(line)
        if href is None:
            continue
        path = href.group(1)
        if line.lstrip().startswith("#"):
            known.add(path)  # commented-out entry: excluded on purpose
            continue
        linked.add(path)
        known.add(path)
        if current is not None:
            current["hrefs"].append(path)
            current["insert_after"] = i
    return lines, sections, linked, known


def scan_chart_dirs(root):
    """List the English chart QMDs on disk, sorted within each directory."""
    pages = []
    for directory in CHART_DIRS:
        dir_path = os.path.join(root, directory)
        if not os.path.isdir(dir_path):
            print(f"warning: chart directory {directory}/ not found", file=sys.stderr)
            continue
        for name in sorted(os.listdir(dir_path)):
            if name.endswith(".qmd") and not name.endswith(".zh.qmd"):
                pages.append(f"{directory}/{name}")
    return pages


def read_frontmatter_title(path):
    """Return the qmd frontmatter `title`, or None when absent/unreadable."""
    try:
        with open(path, encoding="utf-8") as fh:
            head = fh.read(4096)
    except OSError:
        return None
    frontmatter = re.match(r"^---\r?\n(.*?)\r?\n---", head, re.S)
    if frontmatter is None:
        return None
    body = frontmatter.group(1)
    quoted = re.search(r'^title:\s*"([^"]*)"', body, re.M)
    if quoted:
        return quoted.group(1)
    plain = re.search(r"^title:\s*(.+?)\s*$", body, re.M)
    if plain is None:
        return None
    title = plain.group(1)
    if len(title) >= 2 and title[0] == title[-1] and title[0] in "'\"":
        title = title[1:-1]
    return title


def classify_missing(missing, sections, root):
    """Split missing pages into auto-appendable ones and manual ones.

    Auto entries carry (path, title, section index); manual entries carry
    (path, reason).
    """
    auto = []
    manual = []
    for path in missing:
        directory = path.split("/")[0]
        targets = [s for s in sections if any(h.startswith(directory + "/") for h in s["hrefs"])]
        if len(targets) > 1:
            manual.append((path, f"{directory}/ spans {len(targets)} sidebar sections"))
        elif not targets:
            manual.append((path, f"no sidebar section links {directory}/ yet"))
        elif targets[0]["insert_after"] is None:
            manual.append((path, "target section has no entries to append after"))
        else:
            title = read_frontmatter_title(os.path.join(root, path))
            if title is None:
                manual.append((path, "missing frontmatter title"))
            else:
                auto.append((path, title, sections.index(targets[0])))
    return auto, manual


def main():
    parser = argparse.ArgumentParser(description="Sync the _quarto.yml sidebar with chart QMDs on disk.")
    mode = parser.add_mutually_exclusive_group(required=True)
    mode.add_argument("--check", action="store_true", help="exit 1 if the sidebar is out of sync (for CI)")
    mode.add_argument("--update", action="store_true", help="append missing entries to _quarto.yml in place")
    parser.add_argument("--root", default=None, help="repository root (default: the script's repository)")
    args = parser.parse_args()

    root = os.path.abspath(args.root) if args.root else DEFAULT_ROOT
    yml_path = os.path.join(root, "_quarto.yml")
    with open(yml_path, encoding="utf-8") as fh:
        text = fh.read()

    lines, sections, linked, known = parse_sidebar(text)
    disk = scan_chart_dirs(root)
    missing = [p for p in disk if p not in known]
    broken = [h for h in linked if not os.path.exists(os.path.join(root, h))]
    auto, manual = classify_missing(missing, sections, root)

    if args.check:
        for path, title, _ in auto:
            print(f"OUT OF SYNC: {path} missing from sidebar (would append as \"{title}\")")
        for path, reason in manual:
            print(f"OUT OF SYNC: {path} needs manual placement ({reason})")
        for href in broken:
            print(f"OUT OF SYNC: sidebar links missing file {href}")
        if auto or manual or broken:
            print(
                f"--check failed: {len(auto)} missing, {len(manual)} manual, "
                f"{len(broken)} broken link(s)"
            )
            return 1
        print(f"sidebar is in sync with disk ({len(linked)} linked pages)")
        return 0

    # --update: append auto entries bottom-up so earlier indices stay valid.
    insertions = {}
    for path, title, section_idx in auto:
        insertions.setdefault(section_idx, []).append((path, title))
    for section_idx in sorted(insertions, key=lambda i: -sections[i]["insert_after"]):
        position = sections[section_idx]["insert_after"] + 1
        block = []
        for path, title in sorted(insertions[section_idx]):
            block.append(f'{ENTRY_INDENT}- text: "{title}"')
            block.append(f"{HREF_INDENT}href: {path}")
        lines[position:position] = block

    eol = "\r\n" if "\r\n" in text else "\n"
    new_text = eol.join(lines)
    if new_text != text:
        try:
            import yaml
        except ImportError:
            yaml = None
        if yaml is not None:
            try:
                yaml.safe_load(new_text)
            except yaml.YAMLError as exc:
                sys.exit(f"error: updated _quarto.yml does not parse; aborting without writing:\n{exc}")
        with open(yml_path, "w", encoding="utf-8", newline="") as fh:
            fh.write(new_text)

    applied = sum(len(entries) for entries in insertions.values())
    for path, reason in manual:
        print(f"MANUAL ACTION: {path} ({reason})")
    if applied:
        print(f"appended {applied} sidebar entr{'y' if applied == 1 else 'ies'} to _quarto.yml")
    elif manual:
        print("no entries appended; resolve the manual actions above")
    else:
        print(f"sidebar is in sync with disk ({len(linked)} linked pages); no changes")
    return 1 if manual else 0


if __name__ == "__main__":
    sys.exit(main())
