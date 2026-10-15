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
main = build_bundle_mod.main

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
    bundle, _ = build_bundle(en, zh, None, None, tmp_path, "https://site/")
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
    bundle, _ = build_bundle(en, None, None, None, tmp_path, "https://site/")
    dist = [c for c in bundle["categories"] if c["name"] == "Distribution"]
    assert dist and dist[0]["count"] == 2


def test_builder_rejects_short_row_missing_required_fields(tmp_path, qmd_root):
    # Row 2 is truncated: DictReader restval fills missing columns with None,
    # which must not pass through into chart dicts (search crashes on nulls).
    en = tmp_path / "gallery.csv"
    en.write_text(EN_CSV + "3,Broken Chart,https://img/3.png\n")
    with pytest.raises(ValueError, match="required field") as excinfo:
        build_bundle(en, None, None, None, tmp_path, "https://site/")
    assert "Tutorial_url" in str(excinfo.value)


def test_builder_rejects_row_with_empty_required_field(tmp_path, qmd_root):
    en = tmp_path / "gallery.csv"
    en.write_text(EN_CSV.replace(
        '2,Dumbbell Plot,https://img/2.png,https://site/Ranking/Dumbbell.html,"Compare two",Dumbbell,RANKING,Distribution',
        '2,,https://img/2.png,https://site/Ranking/Dumbbell.html,"Compare two",Dumbbell,RANKING,Distribution',
    ))
    with pytest.raises(ValueError, match="required field") as excinfo:
        build_bundle(en, None, None, None, tmp_path, "https://site/")
    assert "Name" in str(excinfo.value)
    assert "Id=2" in str(excinfo.value)


def test_main_exits_nonzero_and_reports_when_gallery_missing(tmp_path, qmd_root, capsys, monkeypatch):
    # I-4: a silent empty bundle (exit 0) would let CI publish content loss.
    monkeypatch.chdir(tmp_path)
    monkeypatch.setattr("sys.argv", ["build-bundle.py", "--out", str(tmp_path / "out"),
                                     "--gallery", str(tmp_path / "absent.csv")])
    with pytest.raises(SystemExit) as excinfo:
        main()
    assert excinfo.value.code != 0
    err = capsys.readouterr().err
    assert "gallery" in err.lower()


def test_main_prints_loud_summary_on_zero_charts(tmp_path, qmd_root, capsys, monkeypatch):
    en = tmp_path / "gallery.csv"
    en.write_text("Id,Name,Image_url,Tutorial_url,Description,Type,Level1,Level2\n")
    monkeypatch.chdir(tmp_path)
    (tmp_path / "files").mkdir()
    (tmp_path / "files" / "gallery_data.csv").write_text(en.read_text())
    monkeypatch.setattr("sys.argv", ["build-bundle.py", "--out", str(tmp_path / "out"),
                                     "--gallery", str(tmp_path / "files" / "gallery_data.csv"),
                                     "--qmd-root", str(tmp_path)])
    with pytest.raises(SystemExit) as excinfo:
        main()
    assert excinfo.value.code != 0
    assert "0 charts" in capsys.readouterr().err


def test_main_reports_pages_whose_qmd_is_missing(tmp_path, qmd_root, capsys, monkeypatch):
    en = tmp_path / "gallery.csv"
    # Dumbbell has no QMD on disk → partial snippet loss surfaces as a loud
    # ⚠️ warning (exit 0); only total content loss exits non-zero.
    en.write_text(EN_CSV)
    monkeypatch.chdir(tmp_path)
    (tmp_path / "files").mkdir(exist_ok=True)
    (tmp_path / "files" / "gallery_data.csv").write_text(EN_CSV)
    monkeypatch.setattr("sys.argv", ["build-bundle.py", "--out", str(tmp_path / "out"),
                                     "--gallery", str(tmp_path / "files" / "gallery_data.csv"),
                                     "--qmd-root", str(tmp_path)])
    main()  # must NOT raise SystemExit: one missing qmd is a warning, not fatal
    err = capsys.readouterr().err
    assert "⚠️" in err
    assert "Dumbbell" in err or "qmd" in err.lower()
    # bundle still written with the healthy chart
    assert (tmp_path / "out" / "bundle.json").exists()


def test_llms_txt_lists_all_charts(tmp_path, qmd_root):
    en = tmp_path / "gallery.csv"
    en.write_text(EN_CSV)
    bundle, _ = build_bundle(en, None, None, None, tmp_path, "https://site/")
    txt = llms_txt(bundle)
    assert "# Bizard" in txt
    assert "Basic Violin Plot" in txt
    assert "https://site/Distribution/ViolinPlot.html" in txt
