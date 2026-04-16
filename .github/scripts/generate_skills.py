#!/usr/bin/env python3
"""
generate_skills.py — Convert Bizard QMD tutorials into AI skill documents.

This script supports two modes:
  1. **LLM mode** (default when API key is available): Uses an LLM API to generate
     high-quality skill documents following the specification in skill-spec.md.
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
    # Remove callout blocks entirely
    prose = re.sub(r'^:::\s*callout-\w+.*?^:::\s*$', '', prose, flags=re.MULTILINE | re.DOTALL)
    prose = re.sub(r'^:::.*$', '', prose, flags=re.MULTILINE)
    for para in prose.split("\n\n"):
        para = para.strip()
        # Skip images, headers, code blocks, and short paragraphs
        if not para or para.startswith("#") or para.startswith("!"):
            continue
        if para.startswith("```"):
            continue
        if len(para) < 20:
            continue
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

    def _clean_block(code):
        lines = [l for l in code.strip().split('\n') if not l.strip().startswith('#|')]
        return '\n'.join(lines).strip()

    # Prefer blocks with 'fig' in label
    for opts, code in blocks:
        meta = _block_meta(opts, code)
        if 'fig' in meta and 'packages' not in meta and 'setup' not in meta:
            return _clean_block(code)

    # Fall back to first non-setup block
    for opts, code in blocks:
        meta = _block_meta(opts, code)
        if 'setup' not in meta and 'session' not in meta and 'data-prep' not in meta:
            return _clean_block(code)

    return _clean_block(blocks[0][1])


def extract_all_code_blocks(content: str, language: str) -> List[str]:
    """Extract all code blocks for a language, excluding setup/package blocks."""
    pattern = CODE_BLOCK_RE.get(language)
    if not pattern:
        return []
    blocks = pattern.findall(content)
    result = []
    for opts, code in blocks:
        meta = (opts + ' ' + ' '.join(re.findall(r'#\|\s*label:\s*(.+)', code))).lower()
        if 'packages' in meta and 'setup' in meta:
            continue
        lines = [l for l in code.strip().split('\n') if not l.strip().startswith('#|')]
        cleaned = '\n'.join(lines).strip()
        if cleaned:
            result.append(cleaned)
    return result


def extract_section_headings(content: str) -> List[str]:
    """Extract section headings from markdown content."""
    return re.findall(r'^###?\s+(.+)$', content, re.MULTILINE)


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
    all_blocks = extract_all_code_blocks(content, language)
    headings = extract_section_headings(content)

    rel_html = str(filepath.with_suffix(".html")).lstrip("./")
    tutorial_url = base_url.rstrip("/") + "/" + rel_html

    # Build use_when description with better fallbacks
    if description:
        use_when = description
    elif category == "Hiplot":
        use_when = f"Create a {title} using R with the Hiplot platform's approach. Suitable for biomedical data visualization with publication-quality output."
    else:
        use_when = f"Create a {title} visualization in {lang_display} for biomedical data analysis and research publications."

    pkg_list = "\n".join(f"- {p}" for p in packages) or "- (see tutorial)"
    fence_lang = {'r': 'r', 'python': 'python', 'julia': 'julia'}.get(language, 'r')

    # Build combined self-contained code from data-prep + first figure block
    combined_code = _build_combined_code(content, language, packages)
    if combined_code:
        code_block = f"```{fence_lang}\n{combined_code}\n```"
    elif code:
        code_block = f"```{fence_lang}\n{code}\n```"
    else:
        code_block = "(See full tutorial for code)"

    # Extract key parameters from the code
    key_params = _extract_key_parameters(content, language, all_blocks)
    params_text = "\n".join(f"- `{k}`: {v}" for k, v in key_params.items()) if key_params else "- See full tutorial for customization options"

    # Generate tips based on the tutorial content
    tips = _generate_tips(content, language, category, headings, all_blocks)
    tips_text = "\n".join(f"- {t}" for t in tips)

    # Build the skill title: strip any existing language tag from the QMD title to avoid
    # duplication (e.g., QMD title "Scatter Plot (Python)" + "(Python)" = "Scatter Plot (Python) (Python)")
    skill_title = re.sub(r'\s*\((R|Python|Julia)\)\s*$', '', title, flags=re.IGNORECASE).strip()

    skill_text = (
        f"# Skill: {skill_title} ({lang_display})\n\n"
        f"## Category\n{category}\n\n"
        f"## When to Use\n{use_when}\n\n"
        f"## Required {lang_display} Packages\n{pkg_list}\n\n"
        f"## Minimal Reproducible Code\n{code_block}\n\n"
        f"## Key Parameters\n{params_text}\n\n"
        f"## Tips\n{tips_text}\n\n"
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


def _build_combined_code(content: str, language: str, packages: List[str]) -> str:
    """Build a combined self-contained code snippet from setup + data-prep + first figure."""
    pattern = CODE_BLOCK_RE.get(language)
    if not pattern:
        return ""
    blocks = pattern.findall(content)
    if not blocks:
        return ""

    parts = []
    # Add library/import loading
    if language == 'r':
        lib_lines = [f"library({p})" for p in packages[:6]]  # Limit to 6 main packages
        if lib_lines:
            parts.append("# Load packages\n" + "\n".join(lib_lines))
    elif language == 'python':
        # Extract actual import lines from the setup block
        for opts, code in blocks:
            meta = (opts + ' ' + ' '.join(re.findall(r'#\|\s*label:\s*(.+)', code))).lower()
            if 'setup' in meta or 'packages' in meta:
                import_lines = [l for l in code.strip().split('\n')
                                if l.strip().startswith('import ') or l.strip().startswith('from ')]
                if import_lines:
                    parts.append("# Load packages\n" + "\n".join(import_lines))
                break
        if not parts:
            imp_lines = []
            for p in packages[:6]:
                imp_lines.append(f"import {p}")
            if imp_lines:
                parts.append("# Load packages\n" + "\n".join(imp_lines))
    elif language == 'julia':
        using_lines = [f"using {p}" for p in packages[:6]]
        # Also add `using Random` if the code references Random functions but Random is not in packages
        # (Random is in JULIA_STDLIB so it's excluded from packages list)
        all_code = "\n".join(code for _, code in blocks)
        if 'Random.' in all_code and 'Random' not in packages:
            using_lines.append("using Random")
        if using_lines:
            parts.append("# Load packages\n" + "\n".join(using_lines))

    # Find and add data preparation block
    for opts, code in blocks:
        meta = (opts + ' ' + ' '.join(re.findall(r'#\|\s*label:\s*(.+)', code))).lower()
        if 'data' in meta or 'prep' in meta:
            cleaned = '\n'.join(l for l in code.strip().split('\n')
                                if not l.strip().startswith('#|'))
            if cleaned.strip():
                parts.append("# Prepare data\n" + cleaned.strip())
            break

    # Find and add first figure block
    for opts, code in blocks:
        meta = (opts + ' ' + ' '.join(re.findall(r'#\|\s*label:\s*(.+)', code))).lower()
        if 'fig' in meta and 'packages' not in meta and 'setup' not in meta:
            cleaned = '\n'.join(l for l in code.strip().split('\n')
                                if not l.strip().startswith('#|'))
            if cleaned.strip():
                parts.append("# Create visualization\n" + cleaned.strip())
            break

    if len(parts) >= 2:
        combined = "\n\n".join(parts)
        # Limit length
        lines = combined.split('\n')
        if len(lines) > 50:
            lines = lines[:50]
            lines.append("# ... (see full tutorial for more)")
        return '\n'.join(lines)
    return ""


def _extract_key_parameters(content: str, language: str,
                             all_blocks: List[str]) -> Dict[str, str]:
    """Extract key parameters from code blocks based on language."""
    params: Dict[str, str] = {}
    combined_code = "\n".join(all_blocks)

    if language == 'r':
        # Common ggplot2 aes parameters
        aes_params = re.findall(r'aes\(([^)]+)\)', combined_code)
        for aes_str in aes_params:
            for mapping in re.findall(r'(\w+)\s*=\s*(\w+)', aes_str):
                if mapping[0] in ('x', 'y', 'fill', 'color', 'colour', 'size', 'shape', 'alpha', 'group'):
                    params[mapping[0]] = f"Maps `{mapping[1]}` to the {mapping[0]} aesthetic"
            if len(params) >= 4:
                break
        # Common geom parameters
        geom_params = {
            'alpha': 'Controls transparency (0 = fully transparent, 1 = opaque)',
            'width': 'Controls element width',
            'position': 'Position adjustment (identity, dodge, stack, fill)',
            'stat': 'Statistical transformation to use',
        }
        for param, desc in geom_params.items():
            if f'{param}' in combined_code and param not in params and len(params) < 8:
                params[param] = desc
        # Theme
        if 'theme_' in combined_code:
            themes = re.findall(r'theme_(\w+)\(\)', combined_code)
            if themes:
                params['theme'] = f"Plot theme; tutorial uses `theme_{themes[0]}()`"

    elif language == 'python':
        # Common matplotlib/seaborn parameters
        py_params = {
            'palette': 'Color palette for the plot (e.g., Set2, viridis, coolwarm)',
            'figsize': 'Figure dimensions as (width, height) in inches',
            'cmap': 'Colormap for continuous color mapping',
            'alpha': 'Transparency level (0–1)',
            'annot': 'Whether to annotate cells with values (True/False)',
            'inner': 'Representation inside violin (box, quartile, point, stick, None)',
            'hue': 'Variable for color grouping',
        }
        for param, desc in py_params.items():
            if param in combined_code and len(params) < 8:
                params[param] = desc

    elif language == 'julia':
        jl_params = {
            'colormap': 'Color scheme for the plot (e.g., :viridis, :RdBu)',
            'markersize': 'Size of scatter plot markers',
            'color': 'Color of plot elements (e.g., :steelblue or (:red, 0.5) for alpha)',
            'linewidth': 'Width of lines in the plot',
            'colorrange': 'Range for color mapping as (min, max) tuple',
            'alpha': 'Transparency level (0–1) via color tuple (color, alpha)',
            'side': 'Side of violin to draw (:left, :right, or both)',
            'width': 'Width of violin or box plot elements',
            'bandwidth': 'Kernel bandwidth for density estimation in violin plots',
            'size': 'Figure size as (width, height) in pixels',
        }
        for param, desc in jl_params.items():
            if param in combined_code and len(params) < 8:
                params[param] = desc
        # Detect color tuple usage (alpha via tuple syntax)
        if re.search(r'\(\s*:\w+\s*,\s*[\d.]+\s*\)', combined_code) and 'alpha' not in params and len(params) < 8:
            params['alpha'] = 'Transparency via color tuple syntax: color=(:steelblue, 0.7)'

    # Ensure at least 3 params
    if len(params) < 3:
        if language == 'r':
            if 'fill' not in params:
                params['fill'] = 'Maps a variable to fill color for group comparison'
            if 'color' not in params:
                params['color'] = 'Maps a variable to outline/point color'
        elif language == 'python':
            if 'figsize' not in params:
                params['figsize'] = 'Figure dimensions as (width, height) in inches'
            if 'alpha' not in params:
                params['alpha'] = 'Transparency level (0–1)'
        elif language == 'julia':
            if 'color' not in params:
                params['color'] = 'Color of plot elements (e.g., :steelblue or (:red, 0.5) for alpha)'
            if 'markersize' not in params:
                params['markersize'] = 'Size of scatter plot markers'
            if len(params) < 3 and 'size' not in params:
                params['size'] = 'Figure size as (width, height) in pixels'

    return dict(list(params.items())[:8])


def _generate_tips(content: str, language: str, category: str,
                   headings: List[str], all_blocks: List[str]) -> List[str]:
    """Generate practical tips based on tutorial content."""
    tips = []
    combined_code = "\n".join(all_blocks)

    # Tips based on visualization sections present
    beautify_headings = [h for h in headings if any(
        kw in h.lower() for kw in ['beautif', 'customiz', 'advanc', 'enhanc', 'polish']
    )]
    if beautify_headings:
        tips.append(f"The tutorial includes a '{beautify_headings[0]}' section with advanced styling options")

    # Language-specific tips
    if language == 'r':
        if 'theme_minimal' in combined_code or 'theme_bw' in combined_code:
            tips.append("Use `theme_minimal()` or `theme_bw()` for clean, publication-ready plots")
        if 'coord_flip' in combined_code:
            tips.append("Use `coord_flip()` for horizontal orientation when labels are long")
        if 'ggsave' in combined_code or 'ggsave' in content:
            tips.append("Export with `ggsave()` for high-resolution output (try width=8, height=6, dpi=300)")
        if 'scale_fill' in combined_code or 'scale_color' in combined_code:
            tips.append("Customize color scales with `scale_fill_manual()` or `scale_color_brewer()`")
        if 'facet_' in combined_code:
            tips.append("Use `facet_wrap()` or `facet_grid()` to create multi-panel plots by group")
        if not tips or len(tips) < 2:
            tips.append("Adjust text size with `theme(text = element_text(size = 14))` for presentations")
    elif language == 'python':
        if 'plt.tight_layout' in combined_code:
            tips.append("Call `plt.tight_layout()` to prevent label overlap")
        if 'sns.' in combined_code:
            tips.append("Seaborn integrates with pandas DataFrames for convenient column-based plotting")
        if 'plt.savefig' in combined_code or 'savefig' in content:
            tips.append("Export with `plt.savefig('plot.png', dpi=300, bbox_inches='tight')`")
        if not tips or len(tips) < 2:
            tips.append("Set style globally with `sns.set_theme(style='whitegrid')` for consistent appearance")
    elif language == 'julia':
        if 'CairoMakie' in combined_code:
            tips.append("CairoMakie produces high-quality vector output suitable for publication")
        tips.append("Save figures with `save(\"plot.png\", fig)` or `save(\"plot.pdf\", fig)`")
        if not tips or len(tips) < 2:
            tips.append("Adjust figure resolution with `Figure(size=(800, 600), figure_padding=20)`")

    # Category-specific tips
    cat_tips = {
        'Distribution': "Consider adding `geom_jitter()` or raw data points alongside distribution plots for small sample sizes",
        'Correlation': "Always check and report the correlation coefficient and p-value alongside visual patterns",
        'Ranking': "Sort categories by value rather than alphabetically for clearer ranking visualization",
        'Composition': "Ensure proportions sum to 100% and consider using a colorblind-friendly palette",
        'DataOverTime': "Highlight key time points or events with vertical reference lines or annotations",
        'Omics': "Include appropriate statistical thresholds (e.g., FDR < 0.05, |log2FC| > 1) in the visualization",
        'Clinics': "Follow CONSORT or STROBE guidelines for clinical data visualization where applicable",
    }
    if category in cat_tips and len(tips) < 5:
        tips.append(cat_tips[category])

    # General quality tip
    if len(tips) < 3:
        tips.append("See the full tutorial for additional customization options and advanced examples")

    return tips[:5]  # Max 5 tips


# ---------------------------------------------------------------------------
# LLM-powered skill generation
# ---------------------------------------------------------------------------

def load_skill_spec() -> str:
    """Load the skill-spec.md specification from the repository root."""
    spec_paths = [
        Path("skill-spec.md"),
        Path(__file__).parent.parent.parent / "skill-spec.md",
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

        # Strip language tag from title for the skill header to avoid duplication
        # (e.g., "Scatter Plot (Python)" becomes "Scatter Plot" before "(Python)" is appended)
        skill_title = re.sub(r'\s*\((R|Python|Julia)\)\s*$', '', title, flags=re.IGNORECASE).strip()

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
- Title: {skill_title}
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

    # Generate unified skill.md from the full index
    generate_unified_skill(index_file, Path("files/gallery_data.csv"),
                           Path("skill.md"))

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


# ── Unified skill.md generation ─────────────────────────────────────

# Category short descriptions used in the skill document
CAT_SHORT = {
    "Distribution": "Distribution shape, spread, and group comparisons "
                    "(violin, box, density, histogram, ridgeline, beeswarm)",
    "Correlation": "Relationships between variables "
                   "(scatter, heatmap, correlogram, bubble, biplot, PCA, UMAP)",
    "Ranking": "Comparison across categories "
               "(bar, lollipop, radar, parallel coordinates, word cloud, upset)",
    "Composition": "Parts of a whole "
                   "(pie, donut, treemap, waffle, Venn, stacked bar)",
    "Proportion": "Proportional relationships and flows "
                  "(Sankey, alluvial, network, chord)",
    "DataOverTime": "Temporal patterns and trends "
                    "(line, area, streamgraph, time series, slope)",
    "Animation": "Animated and interactive visualizations "
                 "(gganimate, ggiraph)",
    "Omics": "Genomics and multi-omics "
             "(volcano, Manhattan, circos, enrichment, pathway, gene structure)",
    "Clinics": "Clinical and epidemiological "
               "(Kaplan-Meier, forest, nomogram, mosaic)",
    "Hiplot": "170+ statistical and bioinformatics templates from Hiplot",
    "Python": "Python-based biomedical visualizations "
              "(matplotlib, seaborn, plotnine)",
    "Julia": "Julia-based visualizations using CairoMakie",
}

CAT_ORDER = [
    "Distribution", "Correlation", "Ranking", "Composition", "Proportion",
    "DataOverTime", "Animation", "Omics", "Clinics", "Hiplot", "Python", "Julia",
]


def generate_unified_skill(index_path: Path, gallery_csv: Path,
                           output_path: Path) -> None:
    """Generate the unified ``skill.md`` — an AI skill instruction document.

    Unlike the old catalog format, this produces a system-prompt-style document
    that instructs AI assistants *how* to use the Bizard gallery data to help
    users create biomedical visualizations.
    """
    import csv as _csv

    if not index_path.exists():
        print(f"  ⚠ Skipping unified skill.md: {index_path} not found")
        return

    all_skills = json.loads(index_path.read_text(encoding="utf-8"))
    if not all_skills:
        print("  ⚠ Skipping unified skill.md: index is empty")
        return

    # Gallery row count
    gallery_count = 0
    if gallery_csv.exists():
        with open(gallery_csv, "r", encoding="utf-8") as fh:
            gallery_count = sum(1 for _ in _csv.reader(fh)) - 1

    # Group by category
    cats: Dict[str, List[dict]] = {}
    for s in all_skills:
        cats.setdefault(s.get("category", "Other"), []).append(s)

    # Determine languages per category for the table
    cat_langs: Dict[str, str] = {}
    for cat, items in cats.items():
        cat_langs[cat] = ", ".join(
            sorted({s.get("language", "R") for s in items})
        )

    L = []  # shorthand for lines
    # ── Title & role ────────────────────────────────────────────────
    L.append("# Bizard — Biomedical Visualization Atlas AI Skill")
    L.append("")
    L.append(
        "You are a biomedical data visualization expert powered by the "
        f"**Bizard** atlas — a comprehensive collection of {len(all_skills)} "
        "reproducible visualization tutorials covering R, Python, and Julia, "
        f"with {gallery_count} curated figure examples from real biomedical "
        "research."
    )

    # ── Capabilities ────────────────────────────────────────────────
    L.append("")
    L.append("## Your Capabilities")
    L.append("")
    L.append(
        "When a user asks for help with data visualization — especially in "
        "the context of biomedical, clinical, or omics research — you should:"
    )
    L.append("")
    L.append(
        "1. **Recommend the right visualization type** based on the user's "
        "data characteristics, research question, and audience."
    )
    L.append(
        "2. **Provide reproducible code** by referencing the Bizard tutorials "
        "and adapting them to the user's specific needs."
    )
    L.append(
        "3. **Link to the full Bizard tutorial** so the user can learn more "
        "and explore advanced customization options."
    )

    # ── How to use gallery_data.csv ─────────────────────────────────
    L.append("")
    L.append("## How to Use `gallery_data.csv`")
    L.append("")
    L.append(
        f"This skill includes a companion data file `gallery_data.csv` with "
        f"{gallery_count} entries. Each row represents one figure example "
        "from a Bizard tutorial. The columns are:"
    )
    L.append("")
    L.append("| Column | Description |")
    L.append("|--------|-------------|")
    L.append("| `Id` | Unique numeric identifier |")
    L.append("| `Name` | Short name of the visualization |")
    L.append("| `Image_url` | Direct URL to the rendered figure image |")
    L.append(
        "| `Tutorial_url` | URL to the specific section of the Bizard tutorial |"
    )
    L.append("| `Description` | What this specific figure demonstrates |")
    L.append(
        '| `Type` | Visualization type (e.g., "Violin Plot", "Volcano Plot") |'
    )
    L.append(
        "| `Level1` | Broad category: BASICS, OMICS, CLINICS, HIPLOT, "
        "PYTHON, JULIA |"
    )
    L.append(
        "| `Level2` | Subcategory (e.g., Distribution, Correlation, Ranking) |"
    )

    # ── Workflow ────────────────────────────────────────────────────
    L.append("")
    L.append("### Workflow for Answering Visualization Requests")
    L.append("")
    L.append(
        "1. **Parse the user's need**: Identify the data type (continuous, "
        "categorical, temporal, genomic, etc.), the comparison type "
        "(distribution, correlation, composition, ranking, flow), and the "
        "target audience (publication, presentation, exploratory)."
    )
    L.append(
        "2. **Search `gallery_data.csv`**: Filter by `Type`, `Level1`, "
        "`Level2`, or keyword-match in `Name`/`Description` to find "
        "relevant examples."
    )
    L.append(
        "3. **Select the best match**: Choose the example(s) that most "
        "closely match the user's requirements. Use `Tutorial_url` to "
        "point them to the full tutorial."
    )
    L.append(
        "4. **Adapt and provide code**: Based on the tutorial, provide "
        "code adapted to the user's data structure. Always include "
        "package installation guards."
    )
    L.append(
        "5. **Offer alternatives**: If multiple visualization types could "
        "work, briefly explain the trade-offs and let the user choose."
    )

    # ── Example query ──────────────────────────────────────────────
    L.append("")
    L.append("### Example Query Resolution")
    L.append("")
    L.append(
        '**User**: "I want to compare gene expression distributions '
        'across 3 cancer subtypes."'
    )
    L.append("")
    L.append("**Your process**:")
    L.append(
        "1. This is a distribution comparison across groups → filter "
        "`Level2 = Distribution`"
    )
    L.append(
        "2. Best matches: Violin Plot (rich distribution shape), Box Plot "
        "(classic, concise), Beeswarm (shows individual points)"
    )
    L.append(
        "3. Recommend Violin Plot as primary, with tutorial link from "
        "`gallery_data.csv`"
    )
    L.append("4. Provide adapted R code using ggplot2 + geom_violin()")

    # ── Categories table ───────────────────────────────────────────
    L.append("")
    L.append("## Visualization Categories")
    L.append("")
    L.append(
        f"The Bizard atlas organizes {len(all_skills)} tutorials into "
        "these categories:"
    )
    L.append("")
    L.append("| Category | Description | Languages |")
    L.append("|----------|-------------|-----------|")
    ordered = list(CAT_ORDER)
    for c in sorted(cats):
        if c not in ordered and c != "Misc":
            ordered.append(c)
    for cat in ordered:
        if cat not in cats:
            continue
        desc = CAT_SHORT.get(cat, f"{cat} visualizations")
        langs = cat_langs.get(cat, "R")
        L.append(f"| **{cat}** | {desc} | {langs} |")
    L.append("")

    # ── Decision guide ─────────────────────────────────────────────
    L.append("## Decision Guide: Choosing the Right Visualization")
    L.append("")
    L.append(
        "When the user describes their goal, map it to the appropriate "
        "category:"
    )
    L.append("")
    L.append("| Research Goal | Recommended Types | Category |")
    L.append("|--------------|-------------------|----------|")
    guide = [
        ("Compare distributions across groups",
         "Violin, Box, Density, Ridgeline, Beeswarm", "Distribution"),
        ("Show relationships between two variables",
         "Scatter, Bubble, Connected Scatter, 2D Density", "Correlation"),
        ("Explore gene/sample correlations",
         "Heatmap, ComplexHeatmap, Correlogram", "Correlation"),
        ("Reduce dimensionality and cluster",
         "PCA, UMAP, tSNE, Biplot", "Correlation"),
        ("Identify differentially expressed genes",
         "Volcano Plot, Multi-Volcano Plot", "Omics"),
        ("Visualize genomic features on chromosomes",
         "Manhattan, Circos, Chromosome, Karyotype", "Omics"),
        ("Show pathway/GO enrichment results",
         "Enrichment Bar/Dot/Bubble Plot, KEGG Pathway", "Omics"),
        ("Display gene structures",
         "Gene Structure Plot, Lollipop Plot, Motif Plot", "Omics"),
        ("Compare values across categories",
         "Bar, Lollipop, Radar, Dumbbell, Parallel Coordinates", "Ranking"),
        ("Show parts of a whole",
         "Pie, Donut, Treemap, Waffle, Stacked Bar", "Composition"),
        ("Depict flows and transitions",
         "Sankey, Alluvial, Network, Chord", "Proportion"),
        ("Show trends over time",
         "Line, Area, Streamgraph, Timeseries", "DataOverTime"),
        ("Animate changes over time",
         "gganimate, plotly, ggiraph", "Animation"),
        ("Show survival curves",
         "Kaplan-Meier Plot", "Clinics"),
        ("Present clinical model results",
         "Forest Plot, Nomogram, Regression Table", "Clinics"),
        ("Create Python-based figures",
         "matplotlib, seaborn, plotnine equivalents", "Python"),
        ("Create Julia-based figures",
         "CairoMakie equivalents", "Julia"),
    ]
    for goal, types, cat in guide:
        L.append(f"| {goal} | {types} | {cat} |")
    L.append("")

    # ── Code conventions ───────────────────────────────────────────
    L.append("## Code Conventions")
    L.append("")
    L.append(
        "When providing code based on Bizard tutorials, always follow "
        "these conventions:"
    )
    L.append("")
    L.append("### R Code")
    L.append("```r")
    L.append("# 1. Package installation guard (ALWAYS include)")
    L.append(
        'if (!requireNamespace("ggplot2", quietly = TRUE)) '
        'install.packages("ggplot2")'
    )
    L.append("")
    L.append("# 2. Library loading")
    L.append("library(ggplot2)")
    L.append("")
    L.append("# 3. Data preparation (prefer public datasets)")
    L.append("# Use built-in: iris, mtcars, ToothGrowth")
    L.append(
        "# Use Bizard hosted: readr::read_csv("
        '"https://bizard-1301043367.cos.ap-guangzhou.myqcloud.com/...")'
    )
    L.append("# Use Bioconductor: TCGA, GEO datasets")
    L.append("")
    L.append("# 4. Visualization code")
    L.append("ggplot(data, aes(x = group, y = value)) +")
    L.append("  geom_violin() +")
    L.append("  theme_minimal()")
    L.append("```")
    L.append("")
    L.append("### Python Code")
    L.append("```python")
    L.append("import matplotlib.pyplot as plt")
    L.append("import seaborn as sns")
    L.append("")
    L.append("# Use public datasets (seaborn built-in, sklearn, etc.)")
    L.append('data = sns.load_dataset("iris")')
    L.append('sns.violinplot(data=data, x="species", y="sepal_length")')
    L.append("plt.show()")
    L.append("```")
    L.append("")
    L.append("### Julia Code")
    L.append("```julia")
    L.append("using CairoMakie, DataFrames, Statistics")
    L.append("")
    L.append("# Use built-in datasets or CSV files")
    L.append("fig = Figure()")
    L.append("ax = Axis(fig[1,1])")
    L.append("violin!(ax, group, values)")
    L.append("fig")
    L.append("```")

    # ── Response format ────────────────────────────────────────────
    L.append("")
    L.append("## Response Format")
    L.append("")
    L.append(
        "When answering visualization requests, structure your response as:"
    )
    L.append("")
    L.append(
        "1. **Recommendation**: Which visualization type(s) to use and why"
    )
    L.append(
        "2. **Code**: Adapted reproducible code based on the relevant "
        "Bizard tutorial"
    )
    L.append(
        "3. **Tutorial Link**: Link to the full Bizard tutorial for "
        "additional options and customization"
    )
    L.append(
        "4. **Alternatives**: Brief mention of other visualization options "
        "if applicable"
    )

    # ── Resources ──────────────────────────────────────────────────
    L.append("")
    L.append("## Key Resources")
    L.append("")
    L.append("- **Website**: https://openbiox.github.io/Bizard/")
    L.append("- **Repository**: https://github.com/openbiox/Bizard")
    L.append(
        f"- **Gallery Data**: See the accompanying `gallery_data.csv` file "
        f"for {gallery_count} figure examples with direct image and "
        "tutorial links"
    )
    L.append(
        "- **License**: CC-BY-NC — Bizard Collaboration Group, "
        "Luo Lab, and Wang Lab"
    )

    output_path.write_text("\n".join(L) + "\n", encoding="utf-8")
    print(f"Unified skill.md written to {output_path} ({len(L)} lines)")


if __name__ == "__main__":
    main()
