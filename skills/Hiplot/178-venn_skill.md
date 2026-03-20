# Skill: Venn (R)

## Category
Hiplot

## When to use
::: callout-note
**Hiplot website**

## Required R packages
- VennDiagram
- data.table
- jsonlite

## Minimal reproducible code
```r
# Venn
col <- c("#E64B35FF","#4DBBD5FF","#00A087FF","#3C5488FF","#F39B7FFF")
p <- venn.diagram(
  data_list, scaled = F, euler.d = F, filename = NULL, col = "black",
  fill = col,
  cex = c(
    1.5, 1.5, 1.5, 1.5, 1.5, 1, 0.8, 1, 0.8, 1, 0.8, 1, 0.8,
    1, 0.8, 1, 0.55, 1, 0.55, 1, 0.55, 1, 0.55, 1, 0.55, 1, 1, 1, 1, 1, 1.5
  ),
  cat.col = col, cat.cex = 1,
  main.fontfamily = "Arial", fontfamily = "Arial", cat.fontface = "bold",
  cat.fontfamily = "Arial", margin = 0.1, main = "Vene Plot", alpha = 0.8
);grid::grid.draw(p)
```

## Full tutorial
https://openbiox.github.io/Bizard/Hiplot/178-venn.html
