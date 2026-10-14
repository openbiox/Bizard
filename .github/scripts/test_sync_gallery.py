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

# {python} mirror of the live ViolinPlot page: on the deployed site
# basic/strip emit `-output-2.png` (a seaborn palette warning becomes the
# first chunk output) while split emits `-output-1.png`.
PY_QMD_EN = """---
title: "Violin Plot"
---

```{python}
#| label: fig-py-violin-basic
#| fig-cap: "Basic violin plot"
plt.show()
```

```{python}
#| label: fig-py-violin-split
#| fig-cap: "Split violin by group"
plt.show()
```

```{python}
#| label: fig-py-violin-strip
#| fig-cap: "Strip plot overlay"
plt.show()
```
"""

PY_QMD_ZH = """---
title: "小提琴图"
---

```{python}
#| label: fig-py-violin-basic
#| fig-cap: "基础小提琴图"
plt.show()
```

```{python}
#| label: fig-py-violin-split
#| fig-cap: "按分组拆分小提琴图"
plt.show()
```

```{python}
#| label: fig-py-violin-strip
#| fig-cap: "带抖动的小提琴图"
plt.show()
```
"""

# An {r} chunk calling an interactive-widget function and a {julia} chunk:
# neither writes a static figure-html PNG (verified against the live site).
WIDGET_QMD_EN = """---
title: "Interactive Charts"
---

```{r}
#| label: fig-ggplotly-basic
#| fig-cap: "Interactive ggplot"
ggplotly(p)
```

```{julia}
#| label: fig-jl-heat
#| fig-cap: "Julia heatmap"
heatmap(xs, ys)
```
"""

WIDGET_QMD_ZH = """---
title: "交互式图表"
---

```{r}
#| label: fig-ggplotly-basic
#| fig-cap: "交互式 ggplot"
ggplotly(p)
```

```{julia}
#| label: fig-jl-heat
#| fig-cap: "Julia 热图"
heatmap(xs, ys)
```
"""


# --------------------------------------------------------------------------
# Chunk parsing
# --------------------------------------------------------------------------

def test_parse_fig_chunks_keeps_only_rendered_figures():
    parse_fig_chunks = sync_gallery.parse_fig_chunks

    figs = parse_fig_chunks(QMD_EN)

    assert [(f["label"], f["caption"]) for f in figs] == [
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

    assert [(f["label"], f["caption"]) for f in parse_fig_chunks(qmd)] == [
        ("fig1Calend", "Calendar"),
        ("fig-1.3size_by", "Sizes"),
    ]


def test_parse_fig_chunks_reports_engine():
    """Each record carries the chunk engine (first word of the fence options)."""
    parse_fig_chunks = sync_gallery.parse_fig_chunks

    qmd = (
        "---\ntitle: \"T\"\n---\n\n"
        "```{r}\n#| label: fig-r-basic\n#| fig-cap: \"R\"\nplot(1)\n```\n"
        "```{python}\n#| label: fig-py-basic\n#| fig-cap: \"Py\"\nplt.show()\n```\n"
        "```{julia}\n#| label: fig-jl-basic\n#| fig-cap: \"Jl\"\nplot(1)\n```\n"
        "```{r, fig.height=5, warning=FALSE}\n#| label: fig-r-opted\n#| fig-cap: \"Opts\"\nplot(2)\n```\n"
    )

    engines = {f["label"]: f["engine"] for f in parse_fig_chunks(qmd)}

    assert engines == {
        "fig-r-basic": "r",
        "fig-py-basic": "python",
        "fig-jl-basic": "julia",
        "fig-r-opted": "r",
    }


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


def write_qmd_page(tmp, directory, page, en_text, zh_text):
    """Write one chart page (en + zh) into the mini repo."""
    os.makedirs(os.path.join(tmp, directory), exist_ok=True)
    with open(os.path.join(tmp, directory, page + ".qmd"), "w") as fh:
        fh.write(en_text)
    with open(os.path.join(tmp, directory, page + ".zh.qmd"), "w") as fh:
        fh.write(zh_text)


def write_gallery_csvs(tmp):
    os.makedirs(os.path.join(tmp, "files"), exist_ok=True)
    for name, text in (("gallery_data.csv", EN_CSV), ("gallery_data_zh.csv", ZH_CSV)):
        with open(os.path.join(tmp, "files", name), "w", newline="") as fh:
            fh.write(text)


def make_root(tmp):
    """Build a mini repo: Omics/iVolcanoPlot.qmd + 2-row gallery CSVs."""
    write_qmd_page(tmp, "Omics", "iVolcanoPlot", QMD_EN, QMD_ZH)
    write_gallery_csvs(tmp)


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


# --------------------------------------------------------------------------
# Rules R1/R2 (follow-up to #340): python-engine URL infix, and fig chunks
# whose output is interactive (or julia) render no static PNG.
# --------------------------------------------------------------------------

def test_collect_missing_derives_python_output_url():
    """R2: python-engine chunks render `<label>-output-1.png`, not `-1.png`."""
    collect_missing = sync_gallery.collect_missing

    with tempfile.TemporaryDirectory() as tmp:
        write_qmd_page(tmp, "Python", "ViolinPlot", PY_QMD_EN, PY_QMD_ZH)
        write_gallery_csvs(tmp)
        missing = collect_missing(tmp)

    assert [m["label"] for m in missing] == [
        "fig-py-violin-basic",
        "fig-py-violin-split",
        "fig-py-violin-strip",
    ]
    split_row = missing[1]
    assert split_row["image_url"] == (
        "https://openbiox.github.io/Bizard/Python/ViolinPlot_files/figure-html/"
        "fig-py-violin-split-output-1.png"
    )
    assert split_row["manual_reason"] is None


def test_collect_missing_routes_widget_and_julia_chunks_to_manual():
    """R1: chunks emitting only interactive widgets (or julia chunks) get a
    manual_reason and no guessed image_url."""
    collect_missing = sync_gallery.collect_missing

    with tempfile.TemporaryDirectory() as tmp:
        write_qmd_page(tmp, "Animation", "Interactivity", WIDGET_QMD_EN, WIDGET_QMD_ZH)
        write_gallery_csvs(tmp)
        missing = collect_missing(tmp)

    assert [m["label"] for m in missing] == ["fig-ggplotly-basic", "fig-jl-heat"]
    for row in missing:
        assert row["image_url"] is None
        assert row["manual_reason"]


def test_collect_missing_routes_known_multi_output_chunks_to_manual():
    """fig-py-violin-basic/strip emit -output-2.png (seaborn palette warning
    becomes the first chunk output) — statically undetectable, so manual."""
    collect_missing = sync_gallery.collect_missing

    with tempfile.TemporaryDirectory() as tmp:
        write_qmd_page(tmp, "Python", "ViolinPlot", PY_QMD_EN, PY_QMD_ZH)
        write_gallery_csvs(tmp)
        missing = collect_missing(tmp)

    for row in (missing[0], missing[2]):
        assert row["image_url"] is None
        assert row["manual_reason"]


def test_check_exits_0_when_only_manual_cases_remain():
    """Permanent manual cases must not fail CI --check forever."""
    check = sync_gallery.check

    with tempfile.TemporaryDirectory() as tmp:
        write_qmd_page(tmp, "Animation", "Interactivity", WIDGET_QMD_EN, WIDGET_QMD_ZH)
        write_gallery_csvs(tmp)
        assert check(tmp) == 0


def test_update_appends_only_placeable_rows_and_reports_manual():
    update = sync_gallery.update

    with tempfile.TemporaryDirectory() as tmp:
        write_qmd_page(tmp, "Animation", "Interactivity", WIDGET_QMD_EN, WIDGET_QMD_ZH)
        write_qmd_page(tmp, "Python", "ViolinPlot", PY_QMD_EN, PY_QMD_ZH)
        write_gallery_csvs(tmp)
        assert update(tmp) == 1  # manual cases remain

        en = open(os.path.join(tmp, "files", "gallery_data.csv"), newline="").read()
        zh = open(os.path.join(tmp, "files", "gallery_data_zh.csv"), newline="").read()

        # only the derivable python row lands, as the next id in both CSVs
        assert "2,Split violin by group," in en
        assert "figure-html/fig-py-violin-split-output-1.png" in en
        assert "2,按分组拆分小提琴图," in zh
        assert "fig-ggplotly-basic" not in en and "fig-jl-heat" not in en


def test_csv_covered_labels_reads_output_infix_urls():
    csv_covered_labels = sync_gallery.csv_covered_labels

    text = (
        "Id,Name,Image_url,Tutorial_url,Description,Type,Level1,Level2\r\n"
        "1,Clustered Heatmap,"
        "https://openbiox.github.io/Bizard/Python/Heatmap_files/figure-html/fig-py-heatmap-cluster-output-1.png,"
        "https://openbiox.github.io/Bizard/Python/Heatmap.html#fig-py-heatmap-cluster,"
        "Clustered heatmap,PYTHON,PYTHON,Heatmap\r\n"
    )

    assert csv_covered_labels(text) == {"fig-py-heatmap-cluster"}


def test_csv_covered_labels_reads_multi_page_urls():
    """Old curated rows may point at `-N.png` (N>=2) of the same chunk with a
    slug anchor; the label still counts as covered so --update does not append
    a near-duplicate row (regression from #340, Ids 802/828)."""
    csv_covered_labels = sync_gallery.csv_covered_labels

    text = (
        "Id,Name,Image_url,Tutorial_url,Description,Type,Level1,Level2\r\n"
        "555,GWAS Circos Plot,"
        "https://openbiox.github.io/Bizard/Omics/GwasSnpPlot_files/figure-html/fig-3GwasSnpManhattan-3.png,"
        "https://openbiox.github.io/Bizard/Omics/GwasSnpPlot.html#manhattan-plot-for-snp-screening,"
        "Manhattan plot for SNP screening,GWAS Circos Plot,OMICS,GwasSnpPlot\r\n"
    )

    assert "fig-3GwasSnpManhattan" in csv_covered_labels(text)


def test_collect_missing_uses_gallery_level2_names():
    """Level2 must match GraphGallery's table filters: DataOverTime pages live
    under "Evolution"; Python/Julia rows use their language name; every other
    BASICS directory's curated rows use the directory name itself."""
    collect_missing = sync_gallery.collect_missing

    with tempfile.TemporaryDirectory() as tmp:
        write_qmd_page(tmp, "DataOverTime", "AreaChart", QMD_EN, QMD_ZH)
        write_gallery_csvs(tmp)
        missing = collect_missing(tmp)

    assert missing[0]["level2"] == "Evolution"

    with tempfile.TemporaryDirectory() as tmp:
        write_qmd_page(tmp, "Python", "ViolinPlot", PY_QMD_EN, PY_QMD_ZH)
        write_gallery_csvs(tmp)
        missing = collect_missing(tmp)

    assert missing[0]["level2"] == "Python"

    for directory in (
        "Distribution",
        "Correlation",
        "Ranking",
        "Composition",
        "Proportion",
        "Animation",
    ):
        with tempfile.TemporaryDirectory() as tmp:
            write_qmd_page(tmp, directory, "SomePage", QMD_EN, QMD_ZH)
            write_gallery_csvs(tmp)
            missing = collect_missing(tmp)
        assert missing[0]["level2"] == directory, directory
