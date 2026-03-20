# Skill: Venn2 (R)

## Category
Hiplot

## When to use
::: callout-note
**Hiplot website**

## Required R packages
- data.table
- jsonlite
- venn

## Minimal reproducible code
```r
# Venn2
col <- c("#E64B35FF","#4DBBD5FF","#00A087FF","#3C5488FF","#F39B7FFF")
venn(x=data_venn, opacity=0.8, ggplot=F, ilabels = TRUE, zcolor=col, box=F)
title(main = "Vene Plot (5 sets)", line = -1)
```

## Full tutorial
https://openbiox.github.io/Bizard/Hiplot/179-venn2.html
