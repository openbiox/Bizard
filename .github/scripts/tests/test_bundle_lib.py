"""Tests for bundle_lib: QMD → code snippet extraction."""
import sys
from pathlib import Path

import pytest

sys.path.insert(0, str(Path(__file__).resolve().parent.parent))

from bundle_lib import extract_code_snippets, extract_data_urls, extract_chart_id


def test_extract_chart_id_from_filename(tmp_path):
    qmd = tmp_path / "ViolinPlot.qmd"
    qmd.write_text("---\ntitle: Violin Plot\n---\n\n# x\n")
    assert extract_chart_id(qmd) == "ViolinPlot"


def test_extract_data_urls_finds_cos_links():
    code = 'read_csv("https://bizard-1301043367.cos.ap-guangzhou.myqcloud.com/data/violin.csv")'
    urls = extract_data_urls(code)
    assert urls == ["https://bizard-1301043367.cos.ap-guangzhou.myqcloud.com/data/violin.csv"]


def test_extract_data_urls_ignores_non_data_links():
    code = 'ggsave("https://example.com/plot.png")'
    assert extract_data_urls(code) == []


def test_extract_code_snippets_r_tutorial(tmp_path):
    cat_dir = tmp_path / "Distribution"
    cat_dir.mkdir()
    qmd = cat_dir / "Demo.qmd"
    qmd.write_text(
        "---\ntitle: Demo\n---\n\n```{r packages setup}\nlibrary(ggplot2)\n```\n\n"
        "```{r fig-1.1Demo}\nggplot(mtcars, aes(x = wt, y = mpg)) + geom_point()\n```\n"
    )
    snippets = extract_code_snippets(qmd, "Demo", "https://openbiox.github.io/Bizard/")
    assert len(snippets) == 1
    snip = snippets[0]
    assert snip["chartId"] == "Demo"
    assert snip["lang"] == "r"
    assert "geom_point" in snip["code"]
    assert snip["packages"] == ["ggplot2"]
    assert snip["sourceAnchor"] == "https://openbiox.github.io/Bizard/Distribution/Demo.html"


def test_extract_code_snippets_empty_qmd(tmp_path):
    qmd = tmp_path / "Empty.qmd"
    qmd.write_text("---\ntitle: Empty\n---\n\nNo code here.\n")
    assert extract_code_snippets(qmd, "Empty", "https://x/") == []
