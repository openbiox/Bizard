#!/usr/bin/env python3
"""Build the MCP content bundle (bundle.json + llms.txt) from gallery CSVs,
recommender keywords, and tutorial QMDs.

Data-source-of-truth contract (spec §3.4): gallery CSV + QMD code + keywords
→ one versioned JSON bundle, published via the site and snapshotted into the
npm package. Must run AFTER sync-gallery.py in CI.

Chart identity: one CSV row = one chart variant; its tutorial page is parsed
from Tutorial_url ("<Category>/<Stem>.html"). Chart id is "{stem}-{Id}" so
variants stay unique while remaining traceable to their page; code snippets
are extracted once per page and replicated per row with that row's chartId.
"""
from __future__ import annotations

import argparse
import csv
import json
import re
from datetime import datetime, timezone
from pathlib import Path
from typing import Dict, List, Optional, Tuple

from bundle_lib import extract_code_snippets

PAGE_URL_RE = re.compile(r"/([^/]+)/([^/#?]+)\.html")

# Fields every gallery row must carry; None/empty here would flow into the
# bundle and crash downstream consumers (search does toLowerCase on these).
REQUIRED_FIELDS = ("Id", "Name", "Type", "Level1", "Level2", "Tutorial_url",
                   "Image_url", "Description")


def validate_row(row: dict, source: str) -> None:
    """Fail loudly when a row is missing required fields (short rows fill
    absent columns with None via DictReader restval)."""
    missing = [f for f in REQUIRED_FIELDS if not (row.get(f) or "").strip()]
    if missing:
        raise ValueError(
            f"{source}: row Id={row.get('Id') or '<missing>'} "
            f"missing required field(s): {', '.join(missing)}"
        )

CATEGORY_NAMES_ZH = {
    "Distribution": "分布", "Correlation": "相关", "Ranking": "排序",
    "Composition": "组成", "Proportion": "比例", "DataOverTime": "时间序列",
    "Animation": "动画", "Omics": "组学", "Clinics": "临床", "Hiplot": "Hiplot",
    "Python": "Python", "Julia": "Julia",
}

CITATION = {
    "paperDoi": "10.1002/imm3.70038",
    "paperUrl": "https://arxiv.org/abs/2503.06845",
    "bibtex": (
        "@article{wang2025bizard,\n"
        "  title={Bizard: an Open Atlas of Biomedical Data Visualization},\n"
        "  author={Wang, Shixiang and others},\n"
        "  journal={iMetaMed},\n"
        "  year={2025},\n"
        "  doi={10.1002/imm3.70038}\n"
        "}"
    ),
    "license": "CC-BY-NC-4.0",
}


def read_csv_rows(path: Optional[Path]) -> List[dict]:
    """Parse a gallery CSV into rows. csv module handles quoted commas."""
    if path is None or not path.exists():
        return []
    with open(path, newline="", encoding="utf-8") as f:
        return list(csv.DictReader(f))


def load_keywords(keywords_en: Optional[Path], keywords_zh: Optional[Path]) -> Optional[dict]:
    if keywords_en is not None and keywords_en.exists():
        return json.loads(keywords_en.read_text(encoding="utf-8"))
    if keywords_zh is not None and keywords_zh.exists():
        return json.loads(keywords_zh.read_text(encoding="utf-8"))
    return None


def build_bundle(gallery_csv: Optional[Path], gallery_zh_csv: Optional[Path],
                 keywords_en: Optional[Path], keywords_zh: Optional[Path],
                 qmd_root: Path, base_url: str) -> dict:
    en_rows = read_csv_rows(gallery_csv)
    for row in en_rows:
        validate_row(row, f"{gallery_csv}")
    zh_by_id = {row["Id"]: row for row in read_csv_rows(gallery_zh_csv)}

    page_snippets: Dict[Tuple[str, str], List[dict]] = {}
    charts: List[dict] = []
    snippets: List[dict] = []
    for row in en_rows:
        page = PAGE_URL_RE.search(row["Tutorial_url"])
        if page:
            category, stem = page.group(1), page.group(2)
            chart_id = f"{stem}-{row['Id']}"
            if (category, stem) not in page_snippets:
                qmd_path = qmd_root / category / f"{stem}.qmd"
                page_snippets[(category, stem)] = (
                    extract_code_snippets(qmd_path, stem, base_url)
                    if qmd_path.is_file() else []
                )
            raw = page_snippets[(category, stem)]
        else:
            chart_id = row["Id"]
            raw = []

        snippets.extend({**s, "chartId": chart_id} for s in raw)
        zh = zh_by_id.get(row["Id"])
        charts.append({
            "id": chart_id,
            "name": row["Name"],
            "nameZh": zh["Name"] if zh else None,
            "type": row["Type"],
            "level1": row["Level1"],
            "level2": row["Level2"],
            "tutorialUrl": row["Tutorial_url"],
            "tutorialUrlZh": zh["Tutorial_url"] if zh else None,
            "imageUrl": row["Image_url"],
            "description": row["Description"],
            "codeRefs": sorted({s["lang"] for s in raw}),
        })

    categories = []
    for name in sorted({c["level2"] for c in charts}):
        categories.append({
            "name": name,
            "nameZh": CATEGORY_NAMES_ZH.get(name),
            "count": sum(1 for c in charts if c["level2"] == name),
        })

    return {
        "meta": {
            "siteVersion": "",
            "bundleVersion": 1,
            "generatedAt": datetime.now(timezone.utc).isoformat(),
            "license": "CC-BY-NC-4.0",
        },
        "categories": categories,
        "charts": charts,
        "codeSnippets": snippets,
        "keywords": load_keywords(keywords_en, keywords_zh),
        "citation": CITATION,
    }


def llms_txt(bundle: dict) -> str:
    lines = [
        "# Bizard",
        "",
        "> An open atlas of biomedical data visualization tutorials (R/Python/Julia).",
        f"> License: {bundle['citation']['license']}. Paper: {bundle['citation']['paperUrl']}",
        "",
    ]
    for cat in bundle["categories"]:
        lines.append(f"## {cat['name']}" + (f" ({cat['nameZh']})" if cat["nameZh"] else ""))
        for c in bundle["charts"]:
            if c["level2"] == cat["name"]:
                lines.append(f"- [{c['name']}]({c['tutorialUrl']})")
        lines.append("")
    return "\n".join(lines)


def main() -> None:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--out", default="files/mcp", help="Output directory")
    parser.add_argument("--base-url", default="https://openbiox.github.io/Bizard/")
    parser.add_argument("--gallery", default="files/gallery_data.csv")
    parser.add_argument("--gallery-zh", default="files/gallery_data_zh.csv")
    parser.add_argument("--keywords", default="files/recommender_keywords.json")
    parser.add_argument("--keywords-zh", default="files/recommender_keywords_zh.json")
    parser.add_argument("--qmd-root", default=".")
    args = parser.parse_args()

    bundle = build_bundle(
        Path(args.gallery), Path(args.gallery_zh),
        Path(args.keywords), Path(args.keywords_zh),
        Path(args.qmd_root), args.base_url,
    )
    out_dir = Path(args.out)
    out_dir.mkdir(parents=True, exist_ok=True)
    (out_dir / "bundle.json").write_text(
        json.dumps(bundle, ensure_ascii=False, indent=2), encoding="utf-8"
    )
    (out_dir / "llms.txt").write_text(llms_txt(bundle), encoding="utf-8")
    print(f"✅ bundle: {len(bundle['charts'])} charts, "
          f"{len(bundle['codeSnippets'])} snippets → {out_dir}/bundle.json")


if __name__ == "__main__":
    main()
