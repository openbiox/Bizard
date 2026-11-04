# Smart Graph Recommender — Deprecation & Keyword Dictionary Guide

## Status

The standalone Smart Graph Recommender widget was removed from the Graph Gallery
pages. Its search capabilities (synonym-aware matching) are now covered by the
gallery search box: query tokens are expanded with the curated synonym
dictionary at search time, so users can still type natural-language terms such
as "versus", "相关性", or "trend" and find matching charts.

## Keyword Dictionary

`files/recommender_keywords.json` (English-first, bilingual) and
`files/recommender_keywords_zh.json` (Chinese-first, bilingual) remain in the
repository and are still actively used:

1. **Gallery search** — `scripts/build_gallery.R` embeds the `synonyms` map
   into the `bz-gallery-config` JSON island when rendering
   `GraphGallery.qmd` / `GraphGallery.zh.qmd`; `files/gallery/gallery.js`
   expands each search token to its synonym group (token → key + siblings) and
   matches charts if any expanded term hits the chart text.
2. **MCP skill bundle** — `.github/scripts/build-bundle.py` (triggered by
   `.github/workflows/mcp-bundle.yml`) packages both files for the Bizard MCP
   server. Do not delete or rename them.

The `intentKeywords` section of both files is only consumed by the MCP bundle
today; the gallery search uses `synonyms` only.

## Maintenance Workflow

1. Edit the `synonyms` section of both JSON files, keeping each entry
   bilingual (English and Chinese equivalents) and concise (5–10 items).
2. Render both pages so the embedded config picks up the new dictionary:
   `quarto render GraphGallery.qmd GraphGallery.zh.qmd --to html`
3. Test a few synonym queries in the rendered search box, then commit.

No copy step is needed for deployment: the synonyms are embedded in the
rendered pages and the CI workflow handles the MCP bundle rebuild.
