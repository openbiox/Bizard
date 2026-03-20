# Skill: DIY GSEA (R)

## Category
Hiplot

## When to use
::: callout-note
**Hiplot website**

## Required R packages
- clusterProfiler
- data.table
- jsonlite

## Minimal reproducible code
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

## Full tutorial
https://openbiox.github.io/Bizard/Hiplot/044-diy-gsea.html
