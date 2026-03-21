#!/usr/bin/env python3
"""
generate_skills.py — Convert Bizard QMD tutorials into AI skill documents.

This script supports two modes:
  1. **LLM mode** (default when API key is available): Uses an LLM API to generate
     high-quality skill documents following the specification in skill.md.
  2. **Offline mode** (--offline or no API key): Uses rule-based extraction as a
     fallback when no LLM API is configured.

Supports R, Python, and Julia tutorials.

Usage:
    # LLM mode (requires API key)
    python .github/scripts/generate_skills.py --verbose

    # Offline fallback
    python .github/scripts/generate_skills.py --offline --verbose

    # Process specific files
    python .github/scripts/generate_skills.py --files Omics/VolcanoPlot.qmd

    # Process changed files only
    python .github/scripts/generate_skills.py --changed-files "Omics/VolcanoPlot.qmd Distribution/ViolinPlot.qmd"
"""

import argparse
import json
import os
import re
import sys
import time
from pathlib import Path
from typing import Dict, List, Optional, Tuple

DEFAULT_BASE_URL = "https://openbiox.github.io/Bizard/"
DEFAULT_MODEL = "gpt-4o-mini"
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
    'unittest', 'dataclasses', 'enum', 'contextlib', 'statistics', 'operator',
    'urllib',
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

KNOWN_CATEGORIES = {
    "Distribution", "Correlation", "Ranking", "Composition",
    "Proportion", "DataOverTime", "Animation", "Omics",
    "Clinics", "Hiplot", "Python", "Julia",
}

# Non-tutorial pages to skip
SKIP_PAGES = {"index", "About", "Skills", "Tutorial", "GraphGallery", "ToolLinks"}


# ---------------------------------------------------------------------------
# Helper functions for extracting metadata from QMD files
# ---------------------------------------------------------------------------

def parse_yaml_field(yaml_text: str, field: str) -> str:
    m = re.search(rf'^{field}\s*:\s*["\']?(.+?)["\']?\s*$', yaml_text, re.MULTILINE)
    return m.group(1).strip() if m else ""


def detect_language(content: str) -> str:
    """Detect the primary programming language of a QMD tutorial."""
    counts = {}
    for lang, pattern in CODE_BLOCK_RE.items():
        blocks = pattern.findall(content)
        real = [b for opts, b in blocks
                if 'packages setup' not in opts and 'session' not in opts.lower()]
        counts[lang] = len(real)
    if not any(counts.values()):
        return 'r'
    return max(counts, key=counts.get)


def extract_packages(content: str, language: str) -> List[str]:
    pkgs: set = set()
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


def extract_description(content: str) -> str:
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


def extract_best_code_block(content: str, language: str) -> str:
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


def detect_category(filepath: Path) -> str:
    for p in filepath.parts:
        if p in KNOWN_CATEGORIES:
            return p
    return "Misc"


# ---------------------------------------------------------------------------
# Offline skill generation (rule-based fallback)
# ---------------------------------------------------------------------------

def qmd_to_skill_offline(filepath: Path, base_url: str = DEFAULT_BASE_URL) -> Dict:
    """Generate a skill document using rule-based extraction (no LLM)."""
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
        f"## When to Use\n{use_when}\n\n"
        f"## Required {lang_display} Packages\n{pkg_list}\n\n"
        f"## Minimal Reproducible Code\n{code_block}\n\n"
        f"## Full Tutorial\n{tutorial_url}\n"
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
        "skill_file": f"skills/{category}/{filepath.stem}_skill.md",
    }


# ---------------------------------------------------------------------------
# LLM-powered skill generation
# ---------------------------------------------------------------------------

def load_skill_spec() -> str:
    """Load the skill.md specification from the repository root."""
    spec_paths = [
        Path("skill.md"),
        Path(__file__).parent.parent.parent / "skill.md",
    ]
    for p in spec_paths:
        if p.exists():
            return p.read_text(encoding="utf-8")
    return ""


class SkillGenerator:
    """Generates skill documents using an LLM API."""

    def __init__(self, api_key: str, model: str = DEFAULT_MODEL,
                 base_url: Optional[str] = None):
        try:
            import openai
        except ImportError:
            print("Error: openai package not installed. Install with: pip install openai",
                  file=sys.stderr)
            sys.exit(1)

        if base_url:
            self.client = openai.OpenAI(api_key=api_key, base_url=base_url)
        else:
            self.client = openai.OpenAI(api_key=api_key)
        self.model = model
        self.skill_spec = load_skill_spec()

    def generate_skill(self, filepath: Path,
                       base_url: str = DEFAULT_BASE_URL) -> Dict:
        """Generate a skill document for a QMD tutorial using the LLM."""
        content = filepath.read_text(encoding="utf-8")
        yaml_match = YAML_RE.match(content)
        yaml_text = yaml_match.group(1) if yaml_match else ""

        title = parse_yaml_field(yaml_text, "title") or filepath.stem
        title = title.strip('"\'')

        language = detect_language(content)
        lang_display = LANG_DISPLAY.get(language, language.capitalize())
        category = detect_category(filepath)
        packages = extract_packages(content, language)

        rel_html = str(filepath.with_suffix(".html")).lstrip("./")
        tutorial_url = base_url.rstrip("/") + "/" + rel_html

        system_prompt = f"""You are an expert at creating AI skill documents for biomedical visualization tutorials.

Follow this specification exactly:

{self.skill_spec}

Important rules:
1. Output ONLY the skill document in Markdown. No explanations or extra text.
2. The skill file MUST follow the exact section structure from the specification.
3. The code MUST be self-contained and produce a visible plot.
4. Use sample/synthetic data — never reference external files.
5. Keep the "When to Use" section to 1–3 clear sentences.
6. List 3–8 key parameters and 2–5 practical tips.
7. Package list must be sorted alphabetically.
"""

        user_prompt = f"""Convert this Bizard QMD tutorial into a skill document.

**Tutorial metadata:**
- Title: {title}
- Language: {lang_display}
- Category: {category}
- Packages detected: {', '.join(packages) if packages else 'none detected'}
- Tutorial URL: {tutorial_url}

**Tutorial content:**

{content[:12000]}
"""

        try:
            response = self.client.chat.completions.create(
                model=self.model,
                messages=[
                    {"role": "system", "content": system_prompt},
                    {"role": "user", "content": user_prompt},
                ],
                temperature=0.3,
                max_tokens=2000,
            )
            skill_text = response.choices[0].message.content.strip()
        except Exception as exc:
            print(f"  LLM API error for {filepath}: {exc}", file=sys.stderr)
            print(f"  Falling back to offline mode for {filepath.name}",
                  file=sys.stderr)
            return qmd_to_skill_offline(filepath, base_url)

        # Extract the "When to use" text from the LLM output
        use_when_match = re.search(
            r'##\s*When to Use\s*\n(.+?)(?=\n##|\Z)', skill_text, re.DOTALL
        )
        use_when = use_when_match.group(1).strip() if use_when_match else ""

        return {
            "name": title,
            "category": category,
            "language": lang_display,
            "packages": packages,
            "use_when": use_when,
            "tutorial_url": tutorial_url,
            "skill": skill_text,
            "source_file": str(filepath),
            "skill_file": f"skills/{category}/{filepath.stem}_skill.md",
        }


# ---------------------------------------------------------------------------
# File discovery
# ---------------------------------------------------------------------------

def iter_qmd_files(path: Path) -> List[Path]:
    """Find all English tutorial QMD files (excluding meta pages and translations)."""
    if path.is_file():
        if path.suffix == ".qmd" and not path.name.endswith(".zh.qmd"):
            return [path]
        return []

    files = []
    for f in sorted(path.rglob("*.qmd")):
        if f.name.endswith(".zh.qmd"):
            continue
        if "Template" in f.parts or f.name.startswith("_"):
            continue
        if f.stem in SKIP_PAGES:
            continue
        files.append(f)
    return files


def resolve_api_config() -> Tuple[Optional[str], Optional[str], str]:
    """Resolve API key, base URL, and model from environment variables."""
    api_key = (os.environ.get("AI_Model_API_KEY")
               or os.environ.get("OPENAI_API_KEY"))
    base_url = os.environ.get("AI_Model_BASE_URL")
    model = os.environ.get("AI_Model_Name", DEFAULT_MODEL)
    return api_key, base_url, model


# ---------------------------------------------------------------------------
# Main entry point
# ---------------------------------------------------------------------------

def main():
    parser = argparse.ArgumentParser(
        description=__doc__,
        formatter_class=argparse.RawDescriptionHelpFormatter,
    )
    parser.add_argument("--input", default=".", help="Input QMD file or directory")
    parser.add_argument("--output", default="skills", help="Output directory")
    parser.add_argument("--base-url", default=DEFAULT_BASE_URL)
    parser.add_argument("--format", choices=["markdown", "json", "both"],
                        default="both")
    parser.add_argument("--offline", action="store_true",
                        help="Use offline rule-based extraction (no LLM)")
    parser.add_argument("--files", nargs="*",
                        help="Specific QMD files to process")
    parser.add_argument("--changed-files",
                        help="Space-separated list of changed QMD files")
    parser.add_argument("--verbose", action="store_true")
    args = parser.parse_args()

    output_dir = Path(args.output)
    output_dir.mkdir(parents=True, exist_ok=True)

    # Determine which files to process
    if args.changed_files:
        qmd_files = [Path(f.strip()) for f in args.changed_files.split()
                     if f.strip().endswith(".qmd")
                     and not f.strip().endswith(".zh.qmd")]
    elif args.files:
        qmd_files = [Path(f) for f in args.files
                     if f.endswith(".qmd") and not f.endswith(".zh.qmd")]
    else:
        qmd_files = iter_qmd_files(Path(args.input))

    if not qmd_files:
        print("No QMD files found to process.", file=sys.stderr)
        sys.exit(0)

    print(f"Processing {len(qmd_files)} QMD file(s)...")

    # Choose generation mode
    api_key, base_url_env, model = resolve_api_config()
    use_llm = not args.offline and api_key is not None

    generator = None
    if use_llm:
        print(f"Using LLM mode (model: {model})")
        generator = SkillGenerator(api_key, model, base_url_env)
    else:
        if not args.offline:
            print("No API key found. Using offline mode.", file=sys.stderr)
        print("Using offline (rule-based) mode")

    skills = []
    errors = []

    for f in qmd_files:
        if not f.exists():
            print(f"  ⚠ File not found: {f}", file=sys.stderr)
            continue
        try:
            if use_llm and generator is not None:
                skill = generator.generate_skill(f, base_url=args.base_url)
                time.sleep(0.5)  # Rate limiting
            else:
                skill = qmd_to_skill_offline(f, base_url=args.base_url)
            skills.append(skill)
            if args.verbose:
                mode = "LLM" if use_llm else "offline"
                print(f"  ✓ {f.name} → {skill['name']} "
                      f"[{skill['category']}] ({skill['language']}) [{mode}]")
        except Exception as exc:
            errors.append((f, exc))
            print(f"  ✗ {f}: {exc}", file=sys.stderr)

    write_json = args.format in ("json", "both")
    write_markdown = args.format in ("markdown", "both")

    # Write individual skill Markdown files
    if write_markdown:
        for skill in skills:
            cat_dir = output_dir / skill["category"]
            cat_dir.mkdir(parents=True, exist_ok=True)
            stem = Path(skill["source_file"]).stem
            out_file = cat_dir / f"{stem}_skill.md"
            out_file.write_text(skill["skill"], encoding="utf-8")
        print(f"\nWrote {len(skills)} skill file(s) to {output_dir}/")

    # Build or update index
    # In full mode (no specific files), replace entirely; in incremental mode, merge
    incremental = bool(args.changed_files or args.files)
    index_file = output_dir / "index.json"

    if incremental and index_file.exists():
        # Load existing index and merge with new skills
        try:
            existing_index = json.loads(index_file.read_text(encoding="utf-8"))
        except (json.JSONDecodeError, OSError):
            existing_index = []

        new_entries = {s["source_file"]: {k: v for k, v in s.items() if k != "skill"}
                       for s in skills}

        updated_index = []
        seen = set()
        for entry in existing_index:
            src = entry.get("source_file", "")
            if src in new_entries:
                updated_index.append(new_entries[src])
                seen.add(src)
            else:
                updated_index.append(entry)
        for src, entry in new_entries.items():
            if src not in seen:
                updated_index.append(entry)
    else:
        # Full replacement
        updated_index = [{k: v for k, v in s.items() if k != "skill"}
                         for s in skills]

    with open(index_file, "w", encoding="utf-8") as fh:
        json.dump(updated_index, fh, ensure_ascii=False, indent=2)
    print(f"Index written to {index_file} ({len(updated_index)} entries)")

    # Write full skills JSON
    if write_json:
        full_file = output_dir / "bizard_skills.json"

        if incremental and full_file.exists():
            # Merge with existing full JSON
            try:
                existing_full = json.loads(full_file.read_text(encoding="utf-8"))
            except (json.JSONDecodeError, OSError):
                existing_full = []

            new_full = {s["source_file"]: s for s in skills}
            updated_full = []
            seen_full = set()
            for entry in existing_full:
                src = entry.get("source_file", "")
                if src in new_full:
                    updated_full.append(new_full[src])
                    seen_full.add(src)
                else:
                    updated_full.append(entry)
            for src, entry in new_full.items():
                if src not in seen_full:
                    updated_full.append(entry)
        else:
            # Full replacement
            updated_full = list(skills)

        with open(full_file, "w", encoding="utf-8") as fh:
            json.dump(updated_full, fh, ensure_ascii=False, indent=2)
        print(f"Full skills JSON written to {full_file} ({len(updated_full)} entries)")

    # Summary
    langs: Dict[str, int] = {}
    for s in skills:
        langs[s['language']] = langs.get(s['language'], 0) + 1
    print(f"\nSummary: {len(skills)} skills processed "
          f"({', '.join(f'{v} {k}' for k, v in sorted(langs.items()))})")

    if errors:
        print(f"\n{len(errors)} file(s) failed:", file=sys.stderr)
        for f, exc in errors:
            print(f"  {f}: {exc}", file=sys.stderr)
        sys.exit(1)


if __name__ == "__main__":
    main()
