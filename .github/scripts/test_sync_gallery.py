"""Tests for sync_gallery.py (issue #339).

Run from the repository root:
    python3 -m pytest .github/scripts/test_sync_gallery.py -v
"""
import importlib.util
import os
import sys

# The script file uses dash naming (sync-gallery.py, like sync-sidebar.py),
# so load it by path instead of a regular import.
_SPEC = importlib.util.spec_from_file_location(
    "sync_gallery", os.path.join(os.path.dirname(os.path.abspath(__file__)), "sync-gallery.py")
)
sync_gallery = importlib.util.module_from_spec(_SPEC)
_SPEC.loader.exec_module(sync_gallery)

# --------------------------------------------------------------------------
# Fixtures: minimal qmd / CSV replicas of the real repo conventions.
# --------------------------------------------------------------------------

QMD_EN = """---
title: "Interactive Volcano Plot"
---

## Basic plot

```{r}
#| label: fig-1.1BasicPlot
#| fig-cap: "Basic interactive volcano plot"
plot(1)
```

## Size mapping

```{r}
#| label: fig-1.3size_by
#| fig-cap: "Map point size by -log10(P) value"
plot(x, y)
```

## Never rendered

```{r}
#| label: fig-skippedEvalFalse
#| eval: false
#| fig-cap: "never rendered"
plot(0)
```

## Not a figure

```{r}
#| label: tbl-summary
#| fig-cap: "a table, not a figure"
summary(1)
```

## Empty caption

```{r}
#| label: fig-emptyCap
#| fig-cap: ""
plot(2)
```
"""

QMD_ZH = """---
title: "交互式火山图"
---

```{r}
#| label: fig-1.1BasicPlot
#| fig-cap: "基础交互式火山图"
plot(1)
```

```{r}
#| label: fig-1.3size_by
#| fig-cap: "按 -log10(P) 值映射点大小"
plot(x, y)
```
"""

EN_CSV = (
    "Id,Name,Image_url,Tutorial_url,Description,Type,Level1,Level2\r\n"
    "1,Basic Interactive Volcano Plot,"
    "https://openbiox.github.io/Bizard/images/Omics/iVolcanoPlot_fig1.png,"
    "https://openbiox.github.io/Bizard/Omics/iVolcanoPlot.html#fig-1.1BasicPlot,"
    "Basic interactive volcano plot using ivolcano,"
    "Interactive Volcano Plot,OMICS,iVolcanoPlot\r\n"
)

ZH_CSV = (
    "Id,Name,Image_url,Tutorial_url,Description,Type,Level1,Level2\r\n"
    "1,基础交互式火山图,"
    "https://openbiox.github.io/Bizard/images/Omics/iVolcanoPlot_fig1.png,"
    "https://openbiox.github.io/Bizard/zh/Omics/iVolcanoPlot.html#fig-1.1BasicPlot,"
    "使用 ivolcano 包绘制基础交互式火山图,"
    "交互式火山图,OMICS,iVolcanoPlot\r\n"
)


# --------------------------------------------------------------------------
# Chunk parsing
# --------------------------------------------------------------------------

def test_parse_fig_chunks_keeps_only_rendered_figures():
    parse_fig_chunks = sync_gallery.parse_fig_chunks

    figs = parse_fig_chunks(QMD_EN)

    assert figs == [
        ("fig-1.1BasicPlot", "Basic interactive volcano plot"),
        ("fig-1.3size_by", "Map point size by -log10(P) value"),
    ]


def test_parse_fig_chunks_accepts_nodash_and_dotted_labels():
    parse_fig_chunks = sync_gallery.parse_fig_chunks

    qmd = (
        "---\ntitle: \"T\"\n---\n\n"
        "```{r}\n#| label: fig1Calend\n#| fig-cap: \"Calendar\"\nplot(1)\n```\n"
        "```{python}\n#| label: fig-1.3size_by\n#| fig-cap: \"Sizes\"\nplot(2)\n```\n"
    )

    assert parse_fig_chunks(qmd) == [
        ("fig1Calend", "Calendar"),
        ("fig-1.3size_by", "Sizes"),
    ]


def test_read_frontmatter_title_handles_quoted_titles():
    read_frontmatter_title = sync_gallery.read_frontmatter_title

    assert read_frontmatter_title(QMD_EN) == "Interactive Volcano Plot"
    assert read_frontmatter_title(QMD_ZH) == "交互式火山图"


def test_parse_hiplot_sections_reads_mathematical_bold_headers():
    """The yml styles HIPLOT headers with mathematical-bold Unicode; NFKC must fold them."""
    parse_hiplot_sections = sync_gallery.parse_hiplot_sections

    yml = (
        "      - section: \"<i class='fa-solid fa-cloud'></i> 𝐇𝐈𝐏𝐋𝐎𝐓\"\n"
        "      - section: \"<i class='fa-solid fa-atom'></i> BASICS\"\n"
        "        contents:\n"
        "          - text: \"Custom Icon Scatter\"\n"
        "            href: Hiplot/035-custom-icon-scatter.qmd\n"
        "          - text: \"D3 Wordcloud\"\n"
        "            href: Hiplot/036-d3-wordcloud.qmd\n"
        "      - section: \"<i class='fa-solid fa-atom'></i> OMICS\"\n"
        "        contents:\n"
        "          - text: \"Funnel\"\n"
        "            href: Hiplot/164-simple-funnel-diagram.qmd\n"
        "      - section: \"<i class='fa-solid fa-atom'></i> CLINICS\"\n"
        "        contents:\n"
        "          - text: \"Stream\"\n"
        "            href: Hiplot/168-streamgraph.qmd\n"
        "      - section: \"<i class='fa-solid fa-atom'></i> MAP\"\n"
        "        contents:\n"
        "          - text: \"Map\"\n"
        "            href: Hiplot/170-map.qmd\n"
        "      - section: \"Other\"\n"
        "        contents:\n"
        "          - text: \"Not sidebar\"\n"
        "            href: Hiplot/999-orphan.qmd\n"
    )
    with tempfile.NamedTemporaryFile("w", suffix=".yml", delete=False) as fh:
        fh.write(yml)
        path = fh.name
    try:
        mapping = parse_hiplot_sections(path)
    finally:
        os.remove(path)

    assert mapping == {
        "Hiplot/035-custom-icon-scatter.qmd": "BASICS",
        "Hiplot/036-d3-wordcloud.qmd": "BASICS",
        "Hiplot/164-simple-funnel-diagram.qmd": "OMICS",
        "Hiplot/168-streamgraph.qmd": "CLINICS",
        "Hiplot/170-map.qmd": "MAP",
    }


# --------------------------------------------------------------------------
# Row derivation (iVolcanoPlot regression case from issue #339)
# --------------------------------------------------------------------------

import tempfile


def make_root(tmp):
    """Build a mini repo: Omics/iVolcanoPlot.qmd + 2-row gallery CSVs."""
    os.makedirs(os.path.join(tmp, "Omics"))
    with open(os.path.join(tmp, "Omics", "iVolcanoPlot.qmd"), "w") as fh:
        fh.write(QMD_EN)
    with open(os.path.join(tmp, "Omics", "iVolcanoPlot.zh.qmd"), "w") as fh:
        fh.write(QMD_ZH)
    os.makedirs(os.path.join(tmp, "files"))
    with open(os.path.join(tmp, "files", "gallery_data.csv"), "w", newline="") as fh:
        fh.write(EN_CSV)
    with open(os.path.join(tmp, "files", "gallery_data_zh.csv"), "w", newline="") as fh:
        fh.write(ZH_CSV)


def test_collect_missing_derives_ivolcano_rows():
    collect_missing = sync_gallery.collect_missing

    with tempfile.TemporaryDirectory() as tmp:
        make_root(tmp)
        missing = collect_missing(tmp)

    assert [(m["dir"], m["page"], m["label"]) for m in missing] == [
        ("Omics", "iVolcanoPlot", "fig-1.3size_by")
    ]
    row = missing[0]
    assert row["id"] == 2
    assert row["name"] == "Map point size by -log10(P) value"
    assert row["image_url"] == (
        "https://openbiox.github.io/Bizard/Omics/iVolcanoPlot_files/figure-html/fig-1.3size_by-1.png"
    )
    assert row["tutorial_url"] == (
        "https://openbiox.github.io/Bizard/Omics/iVolcanoPlot.html#fig-1.3size_by"
    )
    assert row["type"] == "Interactive Volcano Plot"
    assert row["level1"] == "OMICS"
    assert row["level2"] == "iVolcanoPlot"
    assert row["zh_name"] == "按 -log10(P) 值映射点大小"
    assert row["zh_type"] == "交互式火山图"


def test_collect_missing_defaults_zh_text_to_english_seed():
    collect_missing = sync_gallery.collect_missing

    with tempfile.TemporaryDirectory() as tmp:
        make_root(tmp)
        os.remove(os.path.join(tmp, "Omics", "iVolcanoPlot.zh.qmd"))
        missing = collect_missing(tmp)

    assert missing[0]["zh_name"] == missing[0]["name"]
    assert missing[0]["zh_type"] == missing[0]["type"]


def test_check_exits_1_when_rows_missing():
    check = sync_gallery.check

    with tempfile.TemporaryDirectory() as tmp:
        make_root(tmp)
        assert check(tmp) == 1


def test_update_appends_rows_to_both_csvs_preserving_eol():
    update = sync_gallery.update

    with tempfile.TemporaryDirectory() as tmp:
        make_root(tmp)
        assert update(tmp) == 0

        en = open(os.path.join(tmp, "files", "gallery_data.csv"), newline="").read()
        zh = open(os.path.join(tmp, "files", "gallery_data_zh.csv"), newline="").read()

        # append-only: original bytes intact, 1 new row each; the new row
        # keeps the CRLF EOL of the fixture's last line
        assert en.startswith(EN_CSV)
        assert zh.startswith(ZH_CSV)
        assert en.count("\n") == 3 and zh.count("\n") == 3
        assert en.endswith("\r\n") and zh.endswith("\r\n")
        assert "2,Map point size by -log10(P) value" in en
        assert "2,按 -log10(P) 值映射点大小" in zh
        # idempotent: second run changes nothing
        assert update(tmp) == 0
        assert open(os.path.join(tmp, "files", "gallery_data.csv"), newline="").read() == en
