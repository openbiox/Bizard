#!/usr/bin/env python3
"""
generate_skills.py — Convert Bizard QMD tutorials into AI skill documents.

A skill document is a compact Markdown file that:
  - Names the chart and its category
  - Describes when to use the chart
  - Lists required R packages
  - Provides a minimal reproducible code example extracted from the QMD
  - Links back to the full tutorial

Usage:
    # Generate skills for all QMD files in the repo
    python .github/scripts/generate_skills.py

    # Generate for a specific directory
    python .github/scripts/generate_skills.py --input Omics/ --output skills/Omics/

    # Generate a single file skill
    python .github/scripts/generate_skills.py --input Omics/VolcanoPlot.qmd

Options:
    --input   PATH   Input QMD file or directory (default: repo root)
    --output  DIR    Output directory for skill files (default: skills/)
    --base-url URL   Base URL for tutorial links
                     (default: https://openbiox.github.io/Bizard/)
    --format  FORMAT Output format: markdown or json (default: markdown)
    --verbose        Print progress
"""

import argparse
import json
import os
import re
import sys
from pathlib import Path

# ── Constants ─────────────────────────────────────────────────────────────────
DEFAULT_BASE_URL = "https://openbiox.github.io/Bizard/"
YAML_RE          = re.compile(r"^---\s*\n(.*?)\n---", re.DOTALL)
TITLE_RE         = re.compile(r'^title:\s*["\']?(.+?)["\']?\s*$', re.MULTILINE)
AUTHOR_RE        = re.compile(r'^\s*-\s*["\']?\*\*\[Editor\]\*\*\s*(.+?)["\']?\s*$', re.MULTILINE)

# Detect package names from `library(pkg)` or `require(pkg)` calls
LIB_RE   = re.compile(r'\blibrary\(\s*([a-zA-Z0-9_.]+)\s*\)')
REQ_RE   = re.compile(r'\brequireNamespace\(\s*["\']([a-zA-Z0-9_.]+)["\']')

# Detect sections
SECTION_RE = re.compile(r'^(#{1,3})\s+(.+)$', re.MULTILINE)

# Code block extractor
CODE_BLOCK_RE = re.compile(r'```\{r([^}]*)\}(.*?)```', re.DOTALL)

# ── Helpers ──────────────────────────────────────────────────────────────────

def parse_yaml_field(yaml_text: str, field: str) -> str:
    """Extract a simple scalar YAML field value."""
    m = re.search(rf'^{field}\s*:\s*["\']?(.+?)["\']?\s*$', yaml_text, re.MULTILINE)
    return m.group(1).strip() if m else ""


def extract_packages(content: str) -> list[str]:
    """Extract unique package names from library() / requireNamespace() calls."""
    pkgs = set()
    pkgs.update(LIB_RE.findall(content))
    pkgs.update(REQ_RE.findall(content))
    # Remove very common base packages that are assumed
    skip = {"base", "datasets", "utils", "stats", "methods", "grDevices", "graphics"}
    return sorted(pkgs - skip)


def extract_description(content: str) -> str:
    """Extract the first substantive prose paragraph (after YAML, before first ##)."""
    # Remove YAML frontmatter
    body = YAML_RE.sub("", content).strip()
    # Take text before the first ## heading
    parts = re.split(r'^##', body, maxsplit=1, flags=re.MULTILINE)
    prose = parts[0].strip()
    # Take first non-empty paragraph
    for para in prose.split("\n\n"):
        para = para.strip()
        if para and not para.startswith("#") and len(para) > 30:
            # Limit to ~280 chars
            return para[:280].rstrip() + ("…" if len(para) > 280 else "")
    return ""


def extract_first_code_block(content: str, prefer_label: str = "fig") -> str:
    """Return the first R code block (preferring figure blocks)."""
    blocks = CODE_BLOCK_RE.findall(content)
    if not blocks:
        return ""

    # Prefer blocks whose label starts with 'fig'
    for opts, code in blocks:
        if prefer_label in opts.lower():
            return code.strip()

    # Fall back to the first non-setup block
    for opts, code in blocks:
        if "packages setup" not in opts and "session" not in opts:
            return code.strip()

    return blocks[0][1].strip()


def detect_category(filepath: Path) -> str:
    """Infer chart category from the file path."""
    parts = filepath.parts
    known = {
        "Distribution", "Correlation", "Ranking", "Composition",
        "Proportion", "DataOverTime", "Animation", "Omics",
        "Clinics", "Hiplot"
    }
    for p in parts:
        if p in known:
            return p
    return "Misc"


def qmd_to_skill(filepath: Path, base_url: str = DEFAULT_BASE_URL) -> dict:
    """Convert a single QMD file into a skill dictionary."""
    content = filepath.read_text(encoding="utf-8")

    # YAML
    yaml_match = YAML_RE.match(content)
    yaml_text  = yaml_match.group(1) if yaml_match else ""

    title       = parse_yaml_field(yaml_text, "title") or filepath.stem
    # Clean up quotes
    title       = title.strip('"\'')

    category    = detect_category(filepath)
    packages    = extract_packages(content)
    description = extract_description(content)
    code        = extract_first_code_block(content)

    # Build tutorial URL:  base_url + Category/FileName.html
    rel_html = str(filepath.with_suffix(".html")).lstrip("./")
    tutorial_url = base_url.rstrip("/") + "/" + rel_html

    # Use-when: first 2 sentences of description, or a fallback
    use_when = description or f"Visualize {title.lower()} data in a biomedical context."

    # Build skill document text
    pkg_list = "\n".join(f"- {p}" for p in packages[:10]) or "- (see tutorial)"
    code_block = f"```r\n{code}\n```" if code else "(See full tutorial for code)"

    skill_text = f"""# Skill: {title} (R)

## Category
{category}

## When to use
{use_when}

## Required R packages
{pkg_list}

## Minimal reproducible code
{code_block}

## Full tutorial
{tutorial_url}
"""

    return {
        "name":         title,
        "category":     category,
        "packages":     packages,
        "use_when":     use_when,
        "tutorial_url": tutorial_url,
        "skill":        skill_text,
        "source_file":  str(filepath),
    }


def iter_qmd_files(path: Path) -> list[Path]:
    """Yield .qmd files, skipping .zh.qmd translations and Template dir."""
    if path.is_file():
        return [path] if (path.suffix == ".qmd" and not path.name.endswith(".zh.qmd")) else []

    files = []
    for f in sorted(path.rglob("*.qmd")):
        if f.name.endswith(".zh.qmd"):
            continue
        if "Template" in f.parts:
            continue
        if f.name.startswith("_"):
            continue
        files.append(f)
    return files


# ── Main ─────────────────────────────────────────────────────────────────────

def main():
    parser = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    parser.add_argument("--input",   default=".", help="Input QMD file or directory")
    parser.add_argument("--output",  default="skills", help="Output directory")
    parser.add_argument("--base-url",default=DEFAULT_BASE_URL, help="Base URL for tutorial links")
    parser.add_argument("--format",  choices=["markdown", "json", "both"], default="markdown")
    parser.add_argument("--verbose", action="store_true")
    args = parser.parse_args()

    input_path  = Path(args.input)
    output_dir  = Path(args.output)
    output_dir.mkdir(parents=True, exist_ok=True)

    qmd_files = iter_qmd_files(input_path)
    if not qmd_files:
        print(f"No QMD files found in: {input_path}", file=sys.stderr)
        sys.exit(1)

    skills = []
    errors = []

    for f in qmd_files:
        try:
            skill = qmd_to_skill(f, base_url=args.base_url)
            skills.append(skill)
            if args.verbose:
                print(f"✓ {f.name}  →  {skill['name']} [{skill['category']}]")
        except Exception as exc:
            errors.append((f, exc))
            print(f"✗ {f}: {exc}", file=sys.stderr)

    # Write output
    write_json     = args.format in ("json", "both")
    write_markdown = args.format in ("markdown", "both")

    if write_markdown:
        for skill in skills:
            cat_dir  = output_dir / skill["category"]
            cat_dir.mkdir(parents=True, exist_ok=True)
            stem     = Path(skill["source_file"]).stem
            out_file = cat_dir / f"{stem}_skill.md"
            out_file.write_text(skill["skill"], encoding="utf-8")
        print(f"\nWrote {len(skills)} skill files to {output_dir}/")

    # Always write the index JSON (lightweight summary without full skill text)
    index = [
        {k: v for k, v in s.items() if k != "skill"}
        for s in skills
    ]
    index_file = output_dir / "index.json"
    with open(index_file, "w", encoding="utf-8") as fh:
        json.dump(index, fh, ensure_ascii=False, indent=2)
    print(f"Index written to {index_file}")

    if write_json:
        full_file = output_dir / "bizard_skills.json"
        with open(full_file, "w", encoding="utf-8") as fh:
            json.dump(skills, fh, ensure_ascii=False, indent=2)
        print(f"Full skills JSON written to {full_file}")

    if errors:
        print(f"\n{len(errors)} file(s) failed:", file=sys.stderr)
        for f, e in errors:
            print(f"  {f}: {e}", file=sys.stderr)
        sys.exit(1)


if __name__ == "__main__":
    main()
