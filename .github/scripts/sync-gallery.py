#!/usr/bin/env python3
"""Synchronize the gallery CSVs (`files/gallery_data*.csv`) with rendered figures in chart QMDs.

Issue #339: `files/gallery_data.csv` / `files/gallery_data_zh.csv` are
hand-maintained; a chart page without a CSV row silently disappears from
the gallery and the AI skill pack. This script makes "add one qmd" the
only required step:

- Every English chart qmd chunk whose label starts with `fig` and that
  carries a non-empty single-line `fig-cap` corresponds to one rendered
  PNG. Chunks marked `eval: false` are skipped (they never render).
- The existing CSV rows stay the single source of truth for manual
  curation (hand-picked screenshots, edited names, translations). The
  script only APPENDS rows for rendered figures missing from the CSV;
  existing rows are never modified or deleted.
- The zh CSV mirrors every appended English row: `/zh/` tutorial URL,
  the zh page's fig-cap and title as seeds for Name/Description/Type
  (falling back to the English text when no `.zh.qmd` exists).
- Rows that do not reference a rendered `figure-html` PNG or a `#label`
  anchor (manual screenshot rows) cannot be machine-derived, so the
  script never tries to infer figures from them.

Modes:
    python3 .github/scripts/sync-gallery.py --check    # exit 1 if out of sync (CI)
    python3 .github/scripts/sync-gallery.py --update   # append missing rows in place
"""
import argparse
import csv
import io
import os
import re
import sys
import unicodedata

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

CSV_BASE = "https://openbiox.github.io/Bizard"

# Level1 per chart directory.
DIR_LEVEL1 = {
    "Distribution": "BASICS",
    "Correlation": "BASICS",
    "Ranking": "BASICS",
    "Composition": "BASICS",
    "DataOverTime": "BASICS",
    "Proportion": "BASICS",
    "Animation": "BASICS",
    "Omics": "OMICS",
    "Python": "PYTHON",
    "Julia": "JULIA",
    "Clinics": "CLINICS",
}

# Some CLINICS pages derive their Level2 from a category name, not the
# page name; everything else uses the page name as-is.
CLINICS_LEVEL2 = {
    "MetaForestPlot": "Meta-AnalysisForestPlot",
    "RegressionTable": "RegressionAnalysisTable",
}

# Hiplot pages span four gallery sections (BASICS/OMICS/CLINICS/MAP).
# Their Level2 comes from the sidebar section that links the page.
HIPLOT_LEVEL2 = ("BASICS", "OMICS", "CLINICS", "MAP")

CHUNK_RE = re.compile(r"```+\{[^}]*\}\n(.*?)```", re.S)
LABEL_RE = re.compile(r"#\|\s*label:\s*(\S+)")
EVAL_FALSE_RE = re.compile(r"#\|\s*eval:\s*false")
CAP_RE = re.compile(r'#\|\s*fig-cap:\s*"([^"]*)"')


def parse_fig_chunks(qmd_text):
    """Extract (label, fig-cap) pairs for rendered figure chunks.

    A figure chunk carries a label starting with `fig` and a non-empty
    single-line quoted fig-cap; chunks marked `eval: false` never render
    a PNG and are skipped.
    """
    figures = []
    for match in CHUNK_RE.finditer(qmd_text):
        body = match.group(1)
        label = LABEL_RE.search(body)
        cap = CAP_RE.search(body)
        if label is None or cap is None or EVAL_FALSE_RE.search(body):
            continue
        name, caption = label.group(1), cap.group(1)
        if not name.startswith("fig") or not caption:
            continue
        figures.append((name, caption))
    return figures


def read_frontmatter_title(qmd_text):
    """Return the frontmatter `title` of a qmd, or None when absent."""
    frontmatter = re.match(r"^---\r?\n(.*?)\r?\n---", qmd_text, re.S)
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


def derive_level2(directory, page, root):
    """Level2 for a page: directory-specific maps, else the page name."""
    if directory == "Clinics":
        return CLINICS_LEVEL2.get(page, page)
    if directory == "Hiplot":
        return hiplot_level2(os.path.join(root, "_quarto.yml"), directory, page)
    return page


def parse_hiplot_sections(yml_path):
    """Map `Hiplot/<page>.qmd` -> sidebar sub-section name (BASICS/OMICS/CLINICS/MAP).

    The four gallery sub-sections live under the top-level HIPLOT section
    (`- section: "... HIPLOT"` followed by nested `- section: BASICS`...);
    pages are the `href:` lines following each nested header. Headers are
    compared after stripping tags and NFKC-normalizing, because the yml
    styles them with mathematical-bold Unicode characters (𝐇𝐈𝐏𝐋𝐎𝐓) whose
    characters `str.upper()` does not fold back to ASCII.
    """
    with open(yml_path, encoding="utf-8") as fh:
        lines = fh.read().splitlines()
    mapping = {}
    current = None
    in_hiplot = False
    section_re = re.compile(r'^\s*- section: "(.*)"')
    href_re = re.compile(r"href:\s*(Hiplot/\S+\.qmd)")
    tag_re = re.compile(r"<[^>]*>")
    for line in lines:
        sm = section_re.match(line)
        if sm:
            name = unicodedata.normalize("NFKC", tag_re.sub("", sm.group(1))).strip()
            if name.upper() == "HIPLOT":
                in_hiplot = True
                current = None
            elif in_hiplot and name in HIPLOT_LEVEL2:
                current = name
            elif in_hiplot:
                in_hiplot = False
                current = None
            continue
        if current is None:
            continue
        hm = href_re.search(line)
        if hm:
            mapping[hm.group(1)] = current
    return mapping


def hiplot_level2(yml_path, directory, page):
    """Level2 for a Hiplot page from the sidebar; None when unresolvable."""
    return parse_hiplot_sections(yml_path).get(f"{directory}/{page}.qmd")


def collect_missing(root):
    """Derive the CSV rows missing from `files/gallery_data*.csv`.

    Returns a list of row dicts in deterministic order (directory order
    of CHART_DIRS, then page, then chunk order within a page).
    """
    en_csv = os.path.join(root, "files", "gallery_data.csv")
    zh_csv = os.path.join(root, "files", "gallery_data_zh.csv")
    en_text = load_csv_text(en_csv)
    zh_text = load_csv_text(zh_csv)
    en_rows = list(csv.reader(io.StringIO(en_text)))
    zh_rows = list(csv.reader(io.StringIO(zh_text)))
    max_id = max(int(r[0]) for r in en_rows[1:])
    en_count = len(en_rows) - 1
    if len(zh_rows) - 1 != en_count:
        sys.exit(
            f"error: gallery_data.csv has {en_count} rows but gallery_data_zh.csv "
            f"has {len(zh_rows) - 1}; align them before running sync-gallery"
        )

    covered = csv_covered_labels(en_text)

    missing = []
    for directory in CHART_DIRS:
        dir_path = os.path.join(root, directory)
        if not os.path.isdir(dir_path):
            continue
        for name in sorted(os.listdir(dir_path)):
            if not name.endswith(".qmd") or name.endswith(".zh.qmd"):
                continue
            page = name[:-4]
            with open(os.path.join(dir_path, name), encoding="utf-8") as fh:
                text = fh.read()
            zh_path = os.path.join(dir_path, name[:-4] + ".zh.qmd")
            zh_text = None
            if os.path.exists(zh_path):
                with open(zh_path, encoding="utf-8") as fh:
                    zh_text = fh.read()
            title = read_frontmatter_title(text)
            zh_title = read_frontmatter_title(zh_text) if zh_text else None
            for label, caption in parse_fig_chunks(text):
                if label in covered:
                    continue
                level2 = derive_level2(directory, page, root)
                zh_caption = None
                if zh_text:
                    zh_caps = dict(parse_fig_chunks(zh_text))
                    zh_caption = zh_caps.get(label)
                missing.append(
                    {
                        "id": max_id + len(missing) + 1,
                        "dir": directory,
                        "page": page,
                        "label": label,
                        "name": caption,
                        "zh_name": zh_caption or caption,
                        "image_url": f"{CSV_BASE}/{directory}/{page}_files/figure-html/{label}-1.png",
                        "tutorial_url": f"{CSV_BASE}/{directory}/{page}.html#{label}",
                        "zh_tutorial_url": f"{CSV_BASE}/zh/{directory}/{page}.html#{label}",
                        "description": caption,
                        "zh_description": zh_caption or caption,
                        "type": title,
                        "zh_type": zh_title or title,
                        "level1": "HIPLOT" if directory == "Hiplot" else DIR_LEVEL1[directory],
                        "level2": level2,
                    }
                )
    return missing


def format_row(row, zh=False):
    """Render one CSV line (without newline), quoting fields that need it."""
    if zh:
        fields = [
            row["id"],
            row["zh_name"],
            row["image_url"],
            row["zh_tutorial_url"],
            row["zh_description"],
            row["zh_type"],
            row["level1"],
            row["level2"],
        ]
    else:
        fields = [
            row["id"],
            row["name"],
            row["image_url"],
            row["tutorial_url"],
            row["description"],
            row["type"],
            row["level1"],
            row["level2"],
        ]
    buf = io.StringIO()
    csv.writer(buf, lineterminator="").writerow(fields)
    return buf.getvalue()


def load_csv_text(path):
    with open(path, encoding="utf-8", newline="") as fh:
        return fh.read()


def csv_covered_labels(text):
    """Labels already present in a gallery CSV (from figure-html/anchor URLs)."""
    covered = set()
    for r in csv.reader(text.splitlines()):
        if len(r) < 4:
            continue
        m = re.search(r"figure-html/([A-Za-z0-9._-]+?)-1\.png", r[2])
        if m:
            covered.add(m.group(1))
        m2 = re.search(r"\.html#([A-Za-z0-9._-]+)", r[3])
        if m2:
            covered.add(m2.group(1))
    return covered


def append_rows(csv_text, new_lines):
    """Append lines using the EOL of the file's last line (keep trailing EOL)."""
    if not csv_text:
        return "".join(line + "\n" for line in new_lines)
    eol = "\r\n" if csv_text.endswith("\r\n") else "\n"
    return csv_text + "".join(line + eol for line in new_lines)


def check(root):
    """Report out-of-sync rows; return 1 when any exist (CI mode)."""
    missing = collect_missing(root)
    unplaceable = [m for m in missing if m["level2"] is None]
    for m in missing:
        if m["level2"] is None:
            print(f"OUT OF SYNC: {m['dir']}/{m['page']}.qmd#{m['label']} needs manual placement (no Hiplot sidebar section)")
        else:
            print(f"OUT OF SYNC: {m['dir']}/{m['page']}.qmd#{m['label']} missing from gallery (Id {m['id']})")
    if missing:
        print(
            f"--check failed: {len(missing)} missing row(s)"
            + (f", {len(unplaceable)} manual" if unplaceable else "")
        )
        return 1
    print("gallery CSVs are in sync with chart qmds")
    return 0


def update(root):
    """Append missing rows to both CSVs in place; return 1 if manual cases remain."""
    missing = collect_missing(root)
    manual = [m for m in missing if m["level2"] is None]
    applicable = [m for m in missing if m["level2"] is not None]

    paths = {
        False: os.path.join(root, "files", "gallery_data.csv"),
        True: os.path.join(root, "files", "gallery_data_zh.csv"),
    }
    texts = {False: load_csv_text(paths[False]), True: load_csv_text(paths[True])}
    new_lines = {False: [], True: []}
    for row in applicable:
        new_lines[False].append(format_row(row, zh=False))
        new_lines[True].append(format_row(row, zh=True))
    for zh in (False, True):
        if not new_lines[zh]:
            continue
        with open(paths[zh], "w", encoding="utf-8", newline="") as fh:
            fh.write(append_rows(texts[zh], new_lines[zh]))

    for row in manual:
        print(f"MANUAL ACTION: {row['dir']}/{row['page']}.qmd#{row['label']} (no Hiplot sidebar section)")
    if new_lines[False]:
        print(f"appended {len(new_lines[False])} row(s) to both gallery CSVs")
    elif manual:
        print("no rows appended; resolve the manual actions above")
    else:
        print("gallery CSVs are in sync; no changes")
    return 1 if manual else 0


def main():
    parser = argparse.ArgumentParser(description="Sync the gallery CSVs with rendered figures in chart QMDs.")
    mode = parser.add_mutually_exclusive_group(required=True)
    mode.add_argument("--check", action="store_true", help="exit 1 if the gallery CSVs are out of sync (for CI)")
    mode.add_argument("--update", action="store_true", help="append missing rows to the gallery CSVs in place")
    parser.add_argument("--root", default=None, help="repository root (default: the script's repository)")
    args = parser.parse_args()

    root = os.path.abspath(args.root) if args.root else DEFAULT_ROOT
    if args.check:
        return check(root)
    return update(root)


if __name__ == "__main__":
    sys.exit(main())
