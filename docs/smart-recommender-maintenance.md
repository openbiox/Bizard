# Smart Graph Recommender - Maintenance Guide

## Overview

The Smart Graph Recommender widget helps users find suitable charts by matching their visualization goals against a curated keyword dictionary. It runs purely client-side (no LLM, no backend).

## Files Structure

```
files/
├── recommender_keywords.json      # English + Chinese keywords (bilingual)
├── recommender_keywords_zh.json   # Chinese + English keywords (bilingual)
├── gallery_data.csv               # Chart metadata (unchanged)
├── gallery_data_zh.csv            # Chinese chart metadata (unchanged)

GraphGallery.qmd                   # English page with widget
GraphGallery.zh.qmd                # Chinese page with widget
```

## How It Works

1. **User Input** → Text description of visualization goal
2. **Keyword Extraction** → Scan input for all known keywords (Chinese & English)
3. **Synonym Expansion** → Expand matched keywords via synonym dictionary
4. **Intent Matching** → Identify visualization intent categories
5. **Chart Scoring** → Score each chart against matched keywords
6. **Top 5 Results** → Display best matches with thumbnails and links

## Maintenance Tasks

### Adding New Keywords

Edit `files/recommender_keywords.json` and `files/recommender_keywords_zh.json`:

```json
{
  "synonyms": {
    "new_term": ["synonym1", "synonym2", "related_term"]
  },
  "intentKeywords": {
    "new_intent": ["keyword1", "keyword2", "new_term"]
  }
}
```

**Guidelines:**
- Add both Chinese and English equivalents for bilingual support
- Include common abbreviations (e.g., "PCA", "UMAP", "RNA-seq")
- Map specialized bioinfo terms to general chart categories
- Keep synonym lists concise (5-10 items max)

### Adding New Chart Types

When new chart types are added to gallery_data.csv:

1. Add chart type name to synonyms: `"新图表": ["synonym", "english_name"]`
2. Add to relevant intent category: `"组学分析": ["新图表", "关键词"]`
3. Re-render pages: `quarto render GraphGallery.qmd GraphGallery.zh.qmd --to html`
4. Copy updated JSON: `cp files/recommender_keywords*.json _site/files/`

### Testing Keywords

Use Node.js to test keyword matching:

```bash
node -e "
const kw = require('./files/recommender_keywords_zh.json');
const text = '差异表达分析';
// Add test logic here
"
```

Or test in browser console on rendered page.

### Performance Optimization

Current algorithm is O(n*m) where:
- n = number of keywords (~200)
- m = number of charts (~800)

For large expansions:
- Consider caching keyword dictionary (already implemented)
- Limit synonym expansion depth
- Use debounce for input processing (optional enhancement)

## Common Issues

### No Results Found

**Causes:**
- Keyword not in dictionary → Add to synonyms
- Chart Type name mismatch → Check gallery_data.csv Type column
- CSV parsing error → Check for commas in Description field

**Debug:**
- Open browser console, check `recommendChartsZh()` output
- Verify keywords loaded: `keywordsDataZh.synonyms`

### Slow Response

**Causes:**
- Large synonym dictionary (acceptable at ~200 entries)
- Network delay fetching CSV/JSON (cached after first load)

**Solution:**
- Files are cached after first fetch
- Loading indicator shows progress

### XSS Concerns

All user/CSV content is sanitized via `sanitize()` function before innerHTML insertion.

## Updating Workflow

```bash
# 1. Edit keyword dictionaries
vim files/recommender_keywords.json
vim files/recommender_keywords_zh.json

# 2. Render pages
quarto render GraphGallery.qmd GraphGallery.zh.qmd --to html

# 3. Copy updated files
cp files/recommender_keywords*.json _site/files/

# 4. Test locally
python3 -m http.server 8080 --directory _site

# 5. Commit
git add files/recommender_keywords*.json GraphGallery*.qmd
git commit -m "maint: update recommender keywords"
```

## Statistics

- **Keywords**: 201 synonym entries
- **Intent Categories**: 143 intent mappings
- **Charts Covered**: 798 charts
- **Languages**: Chinese + English (bilingual)
- **Coverage**: 50+ chart types, 20+ bioinfo scenarios

## Contact

For issues or suggestions, update the keyword dictionaries following the workflow above.