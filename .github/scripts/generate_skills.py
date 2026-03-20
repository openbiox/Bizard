#!/usr/bin/env python3
"""
generate_skills.py — Convert Bizard QMD tutorials into AI skill documents.

Supports R, Python, and Julia tutorials. A skill document is a compact Markdown
file that names the chart, describes when to use it, lists required packages,
provides minimal reproducible code, and links back to the full tutorial.

Usage:
    python .github/scripts/generate_skills.py
    python .github/scripts/generate_skills.py --input Omics/ --output skills/Omics/
    python .github/scripts/generate_skills.py --format both --verbose
"""

import argparse
import json
import re
import sys
from pathlib import Path

DEFAULT_BASE_URL = "https://openbiox.github.io/Bizard/"
YAML_RE = re.compile(r"^---\s*\n(.*?)\n---", re.DOTALL)

# R package detectors
R_LIB_RE = re.compile(r'\blibrary\(\s*["\']?([a-zA-Z0-9_.]+)["\']?\s*\)')
R_REQ_RE = re.compile(r'\brequireNamespace\(\s*["\']([a-zA-Z0-9_.]+)["\']')
R_INSTALL_RE = re.compile(
    r'(?:install\.packages|pkg_install|BiocManager::install)\(\s*["\']([a-zA-Z0-9_.]+)["\']'
)

# Python package detectors
PY_IMPORT_RE = re.compile(r'^\s*import\s+(\w+)', re.MULTILINE)
PY_FROM_RE = re.compile(r'^\s*from\s+(\w+)', re.MULTILINE)
PYTHON_STDLIB = {
    'os', 'sys', 're', 'math', 'json', 'csv', 'io', 'pathlib', 'collections',
    'itertools', 'functools', 'typing', 'abc', 'datetime', 'time', 'random',
    'string', 'struct', 'zlib', 'hashlib', 'copy', 'warnings', 'logging',
    'argparse', 'subprocess', 'shutil', 'glob', 'tempfile', 'textwrap',
    'unittest', 'dataclasses', 'enum', 'contextlib', 'statistics',
}

# Julia package detectors
JL_USING_RE = re.compile(r'^\s*using\s+(.+)$', re.MULTILINE)
JL_IMPORT_RE = re.compile(r'^\s*import\s+(\w+)', re.MULTILINE)
JULIA_STDLIB = {
    'Base', 'Core', 'Main', 'Printf', 'LinearAlgebra', 'Statistics',
    'Random', 'Dates', 'Test', 'Pkg', 'InteractiveUtils', 'Markdown',
    'REPL', 'Distributed', 'SparseArrays', 'DelimitedFiles',
}

R_BASE_SKIP = {"base", "datasets", "utils", "stats", "methods", "grDevices", "graphics"}

# Code block patterns per language
CODE_BLOCK_RE = {
    'r': re.compile(r'```\{r([^}]*)\}(.*?)```', re.DOTALL),
    'python': re.compile(r'```\{python([^}]*)\}(.*?)```', re.DOTALL),
    'julia': re.compile(r'```\{julia([^}]*)\}(.*?)```', re.DOTALL),
}

LANG_DISPLAY = {'r': 'R', 'python': 'Python', 'julia': 'Julia'}


def parse_yaml_field(yaml_text, field):
    m = re.search(rf'^{field}\s*:\s*["\']?(.+?)["\']?\s*$', yaml_text, re.MULTILINE)
    return m.group(1).strip() if m else ""


def detect_language(content):
    """Detect the primary programming language of a QMD tutorial."""
    counts = {}
    for lang, pattern in CODE_BLOCK_RE.items():
        blocks = pattern.findall(content)
        real = [b for opts, b in blocks if 'packages setup' not in opts and 'session' not in opts.lower()]
        counts[lang] = len(real)
    if not any(counts.values()):
        return 'r'
    return max(counts, key=counts.get)


def extract_packages(content, language):
    pkgs = set()
    if language == 'r':
        pkgs.update(R_LIB_RE.findall(content))
        pkgs.update(R_REQ_RE.findall(content))
        pkgs.update(R_INSTALL_RE.findall(content))
        pkgs -= R_BASE_SKIP
    elif language == 'python':
        blocks = CODE_BLOCK_RE['python'].findall(content)
        for _, block in blocks:
            pkgs.update(PY_IMPORT_RE.findall(block))
            pkgs.update(PY_FROM_RE.findall(block))
        pkgs -= PYTHON_STDLIB
    elif language == 'julia':
        blocks = CODE_BLOCK_RE['julia'].findall(content)
        for _, block in blocks:
            for m in JL_USING_RE.finditer(block):
                for p in m.group(1).split(','):
                    p = p.strip().split(':')[0].strip()
                    if p:
                        pkgs.add(p)
            pkgs.update(JL_IMPORT_RE.findall(block))
        pkgs -= JULIA_STDLIB
    return sorted(pkgs)


def extract_description(content):
    body = YAML_RE.sub("", content).strip()
    parts = re.split(r'^##', body, maxsplit=1, flags=re.MULTILINE)
    prose = parts[0].strip()
    for para in prose.split("\n\n"):
        para = para.strip()
        if para and not para.startswith("#") and not para.startswith("!") and len(para) > 20:
            if len(para) > 500:
                return para[:497].rstrip() + "..."
            return para
    return ""


def extract_best_code_block(content, language):
    pattern = CODE_BLOCK_RE.get(language)
    if not pattern:
        return ""
    blocks = pattern.findall(content)
    if not blocks:
        return ""

    def _block_meta(opts, code):
        labels = re.findall(r'#\|\s*label:\s*(.+)', code)
        return (opts + ' ' + ' '.join(labels)).lower()

    # Prefer blocks with 'fig' in label
    for opts, code in blocks:
        meta = _block_meta(opts, code)
        if 'fig' in meta and 'packages' not in meta and 'setup' not in meta:
            lines = [l for l in code.strip().split('\n') if not l.strip().startswith('#|')]
            return '\n'.join(lines).strip()

    # Fall back to first non-setup block
    for opts, code in blocks:
        meta = _block_meta(opts, code)
        if 'setup' not in meta and 'session' not in meta and 'data-prep' not in meta:
            lines = [l for l in code.strip().split('\n') if not l.strip().startswith('#|')]
            return '\n'.join(lines).strip()

    return blocks[0][1].strip()


def detect_category(filepath):
    known = {
        "Distribution", "Correlation", "Ranking", "Composition",
        "Proportion", "DataOverTime", "Animation", "Omics",
        "Clinics", "Hiplot", "Python", "Julia"
    }
    for p in filepath.parts:
        if p in known:
            return p
    return "Misc"


def qmd_to_skill(filepath, base_url=DEFAULT_BASE_URL):
    content = filepath.read_text(encoding="utf-8")
    yaml_match = YAML_RE.match(content)
    yaml_text = yaml_match.group(1) if yaml_match else ""

    title = parse_yaml_field(yaml_text, "title") or filepath.stem
    title = title.strip('"\'')

    language = detect_language(content)
    lang_display = LANG_DISPLAY.get(language, language.capitalize())
    category = detect_category(filepath)
    packages = extract_packages(content, language)
    description = extract_description(content)
    code = extract_best_code_block(content, language)

    rel_html = str(filepath.with_suffix(".html")).lstrip("./")
    tutorial_url = base_url.rstrip("/") + "/" + rel_html
    use_when = description or f"Visualize {title.lower()} data in a biomedical context."

    pkg_list = "\n".join(f"- {p}" for p in packages) or "- (see tutorial)"
    fence_lang = {'r': 'r', 'python': 'python', 'julia': 'julia'}.get(language, 'r')
    code_block = f"```{fence_lang}\n{code}\n```" if code else "(See full tutorial for code)"

    skill_text = (
        f"# Skill: {title} ({lang_display})\n\n"
        f"## Category\n{category}\n\n"
        f"## When to use\n{use_when}\n\n"
        f"## Required {lang_display} packages\n{pkg_list}\n\n"
        f"## Minimal reproducible code\n{code_block}\n\n"
        f"## Full tutorial\n{tutorial_url}\n"
    )

    return {
        "name": title,
        "category": category,
        "language": lang_display,
        "packages": packages,
        "use_when": use_when,
        "tutorial_url": tutorial_url,
        "skill": skill_text,
        "source_file": str(filepath),
    }


def iter_qmd_files(path):
    path = Path(path)
    if path.is_file():
        return [path] if (path.suffix == ".qmd" and not path.name.endswith(".zh.qmd")) else []
    files = []
    for f in sorted(path.rglob("*.qmd")):
        if f.name.endswith(".zh.qmd") or "Template" in f.parts or f.name.startswith("_"):
            continue
        files.append(f)
    return files


def main():
    parser = argparse.ArgumentParser(description=__doc__,
        formatter_class=argparse.RawDescriptionHelpFormatter)
    parser.add_argument("--input", default=".", help="Input QMD file or directory")
    parser.add_argument("--output", default="skills", help="Output directory")
    parser.add_argument("--base-url", default=DEFAULT_BASE_URL)
    parser.add_argument("--format", choices=["markdown", "json", "both"], default="markdown")
    parser.add_argument("--verbose", action="store_true")
    args = parser.parse_args()

    input_path = Path(args.input)
    output_dir = Path(args.output)
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
                print(f"  ✓ {f.name} → {skill['name']} [{skill['category']}] ({skill['language']})")
        except Exception as exc:
            errors.append((f, exc))
            print(f"  ✗ {f}: {exc}", file=sys.stderr)

    write_json = args.format in ("json", "both")
    write_markdown = args.format in ("markdown", "both")

    if write_markdown:
        for skill in skills:
            cat_dir = output_dir / skill["category"]
            cat_dir.mkdir(parents=True, exist_ok=True)
            stem = Path(skill["source_file"]).stem
            out_file = cat_dir / f"{stem}_skill.md"
            out_file.write_text(skill["skill"], encoding="utf-8")
        print(f"\nWrote {len(skills)} skill files to {output_dir}/")

    index = [{k: v for k, v in s.items() if k != "skill"} for s in skills]
    index_file = output_dir / "index.json"
    with open(index_file, "w", encoding="utf-8") as fh:
        json.dump(index, fh, ensure_ascii=False, indent=2)
    print(f"Index written to {index_file}")

    if write_json:
        full_file = output_dir / "bizard_skills.json"
        with open(full_file, "w", encoding="utf-8") as fh:
            json.dump(skills, fh, ensure_ascii=False, indent=2)
        print(f"Full skills JSON written to {full_file}")

    langs = {}
    for s in skills:
        langs[s['language']] = langs.get(s['language'], 0) + 1
    print(f"\nSummary: {len(skills)} skills ({', '.join(f'{v} {k}' for k, v in sorted(langs.items()))})")

    if errors:
        print(f"\n{len(errors)} file(s) failed:", file=sys.stderr)
        sys.exit(1)


if __name__ == "__main__":
    main()
