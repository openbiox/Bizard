# Skill: Corrplot Big Data (R)

## Category
Hiplot

## When to use
::: callout-note
**Hiplot website**

## Required R packages
- ComplexHeatmap
- data.table
- jsonlite

## Minimal reproducible code
```r
# Corrplot Big Data
p <- ComplexHeatmap::Heatmap(
  corr, col = colorRampPalette(c("#4477AA","#FFFFFF","#BB4444"))(50),
  clustering_distance_rows = "euclidean",
  clustering_method_rows = "ward.D2",
  clustering_distance_columns = "euclidean",
  clustering_method_columns = "ward.D2",
  show_column_dend = FALSE, show_row_dend = FALSE,
  column_names_gp = gpar(fontsize = 8),
  row_names_gp = gpar(fontsize = 8)
)

p
```

## Full tutorial
https://openbiox.github.io/Bizard/Hiplot/013-big-corrplot.html
