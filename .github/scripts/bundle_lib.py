"""Shared extraction helpers for the MCP content bundle.

Reuses code-block/package extractors from generate_skills.py so the two
scripts stay in lockstep (spec §10: extract functions shared via import,
never copied).
"""
from __future__ import annotations

import re
from pathlib import Path
from typing import List

from generate_skills import (
    CODE_BLOCK_RE,
    extract_all_code_blocks,
    extract_packages,
)

DATA_URL_RE = re.compile(r'https?://[^\s"\'\)]+\.(?:csv|tsv|rds|txt|xlsx?)(?=[\s"\'\)]|$)', re.IGNORECASE)


def extract_chart_id(qmd_path: Path) -> str:
    """Chart id is the QMD filename stem (matches gallery_data.csv Name↔file convention)."""
    return qmd_path.stem


def extract_data_urls(code: str) -> List[str]:
    """Find example-data URLs (csv/tsv/rds/txt/xlsx) referenced in code."""
    return [m.group(0).rstrip(".,;") for m in DATA_URL_RE.finditer(code)]


def _category_dir(qmd_path: Path) -> str:
    return qmd_path.parent.name


def _tutorial_url(qmd_path: Path, base_url: str) -> str:
    category = _category_dir(qmd_path)
    stem = qmd_path.stem
    return f"{base_url.rstrip('/')}/{category}/{stem}.html"


def extract_code_snippets(qmd_path: Path, chart_id: str, base_url: str) -> List[dict]:
    """Extract runnable code snippets per language from a tutorial QMD.

    Excludes setup/package blocks (same filter as extract_all_code_blocks).
    Each snippet carries its language, detected packages, and any example
    data URLs found in the code.
    """
    content = qmd_path.read_text(encoding="utf-8")
    tutorial_url = _tutorial_url(qmd_path, base_url)
    snippets: List[dict] = []
    for lang in ("r", "python", "julia"):
        blocks = extract_all_code_blocks(content, lang)
        if not blocks:
            continue
        packages = extract_packages(content, lang)
        for code in blocks:
            snippets.append({
                "chartId": chart_id,
                "lang": lang,
                "code": code,
                "packages": packages,
                "dataUrls": extract_data_urls(code),
                "sourceAnchor": tutorial_url,
            })
    return snippets
