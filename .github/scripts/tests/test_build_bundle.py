"""Tests for build-bundle.py: CSV → bundle.json + llms.txt."""
import sys
from pathlib import Path

import pytest

sys.path.insert(0, str(Path(__file__).resolve().parent.parent))

import importlib.util

_spec = importlib.util.spec_from_file_location(
    "build_bundle", Path(__file__).resolve().parent.parent / "build-bundle.py"
)
build_bundle_mod = importlib.util.module_from_spec(_spec)
_spec.loader.exec_module(build_bundle_mod)
build_bundle = build_bundle_mod.build_bundle
llms_txt = build_bundle_mod.llms_txt

EN_CSV = """Id,Name,Image_url,Tutorial_url,Description,Type,Level1,Level2
1,"Basic Violin Plot",https://img/1.png,https://site/Distribution/ViolinPlot.html,"A plot, with comma",Violin Plot,BASICS,Distribution
2,Dumbbell Plot,https://img/2.png,https://site/Ranking/Dumbbell.html,"Compare two",Dumbbell,RANKING,Distribution
"""

ZH_CSV = """Id,Name,Image_url,Tutorial_url,Description,Type,Level1,Level2
1,基础小提琴图,https://img/1.png,https://zh.site/ViolinPlot.html,"小提琴图",Violin Plot,BASICS,Distribution
"""


@pytest.fixture
def qmd_root(tmp_path):
    d = tmp_path / "Distribution"
    d.mkdir()
    (d / "ViolinPlot.qmd").write_text(
        "---\ntitle: Violin Plot\n---\n\n"
        "```{r packages setup}\nlibrary(ggplot2)\n```\n\n"
        "```{r fig-1.1BasicViolin}\nlibrary(ggplot2)\nggplot(df, aes(x)) + geom_violin()\n"
        'read_csv("https://bizard-1301043367.cos.ap-guangzhou.myqcloud.com/v.csv")\n```\n'
    )
    return tmp_path


def test_bundle_roundtrip_real_schema(tmp_path, qmd_root):
    en = tmp_path / "gallery.csv"
    zh = tmp_path / "gallery_zh.csv"
    en.write_text(EN_CSV)
    zh.write_text(ZH_CSV)
    bundle = build_bundle(en, zh, None, None, tmp_path, "https://site/")
    assert len(bundle["charts"]) == 2
    # comma inside quoted description survives
    assert bundle["charts"][0]["description"] == "A plot, with comma"
    # zh join by Id
    assert bundle["charts"][0]["nameZh"] == "基础小提琴图"
    assert bundle["charts"][1]["nameZh"] is None
    # code snippets extracted with data urls
    assert bundle["codeSnippets"][0]["packages"] == ["ggplot2"]
    assert any("cos.ap-guangzhou" in u for u in bundle["codeSnippets"][0]["dataUrls"])
    assert bundle["citation"]["paperDoi"] == "10.1002/imm3.70038"
    assert bundle["meta"]["license"] == "CC-BY-NC-4.0"


def test_bundle_categories_counts(tmp_path, qmd_root):
    en = tmp_path / "gallery.csv"
    en.write_text(EN_CSV)
    bundle = build_bundle(en, None, None, None, tmp_path, "https://site/")
    dist = [c for c in bundle["categories"] if c["name"] == "Distribution"]
    assert dist and dist[0]["count"] == 2


def test_llms_txt_lists_all_charts(tmp_path, qmd_root):
    en = tmp_path / "gallery.csv"
    en.write_text(EN_CSV)
    bundle = build_bundle(en, None, None, None, tmp_path, "https://site/")
    txt = llms_txt(bundle)
    assert "# Bizard" in txt
    assert "Basic Violin Plot" in txt
    assert "https://site/Distribution/ViolinPlot.html" in txt
