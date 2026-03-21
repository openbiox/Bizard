# Skill: DIY GSEA (R)

## Category
Hiplot

## When to Use
::: callout-note
**Hiplot website**

## Required R Packages
- clusterProfiler
- data.table
- jsonlite

## Minimal Reproducible Code
```r
# DIY GSEA
y <- clusterProfiler::GSEA(geneList, TERM2GENE = term, pvalueCutoff = 1)
p <- gseaplot(
  y,
  y@result$Description[1],
  color = "#000000",
  by = "runningScore",
  color.line = "#4CAF50",
  color.vline= "#FA5860",
  title = "DIY GSEA Plot",
  )

p
```

## Full Tutorial
https://openbiox.github.io/Bizard/Hiplot/044-diy-gsea.html
