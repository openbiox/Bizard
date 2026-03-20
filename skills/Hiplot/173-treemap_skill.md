# Skill: Treemap (R)

## Category
Hiplot

## When to use
::: callout-note
**Hiplot website**

## Required R packages
- data.table
- jsonlite
- treemap

## Minimal reproducible code
```r
# Treemap
treemap(data, index = colnames(data)[1], vSize = colnames(data)[2],
        vColor = colnames(data)[1], type = "index", title = "", 
        algorithm = "pivotSize", sortID = colnames(data)[1], border.lwds = 1,
        fontcolor.labels = "#000000", inflate.labels = F, overlap.labels = 0.5,
        fontfamily.title = "Arial", fontfamily.legend = "Arial",
        fontfamily.labels = "Arial", 
        palette = c("#E64B35FF","#4DBBD5FF","#00A087FF","#3C5488FF","#F39B7FFF"), 
        aspRatio = 6 / 6)
```

## Full tutorial
https://openbiox.github.io/Bizard/Hiplot/173-treemap.html
