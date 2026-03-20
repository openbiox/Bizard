# Skill: GOBar Plot (R)

## Category
Hiplot

## When to use
::: callout-note
**Hiplot website**

## Required R packages
- GOplot
- data.table
- jsonlite

## Minimal reproducible code
```r
# GOBar Plot
p <- GOBar(data, display = "multiple", order.by.zscore = T,
           title = "GO Enrichment Barplot ", 
           zsc.col = c("#EF8A62","#F7F7F7","#67A9CF")) + 
    theme(plot.title = element_text(hjust = 0.5),
          axis.text.x = element_text(size = 8))

p
```

## Full tutorial
https://openbiox.github.io/Bizard/Hiplot/077-gobar.html
