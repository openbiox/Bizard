# Skill: GOCircle Plot (R)

## Category
Hiplot

## When to Use
::: callout-note
**Hiplot website**

## Required R Packages
- GOplot
- data.table
- ggplotify
- jsonlite

## Minimal Reproducible Code
```r
# GOCircle Plot
p <- function () {
  GOCircle(data, title = "GO Enrichment Circleplot",
           nsub = 10, rad1 = 2, rad2 = 3, table.legend = T, label.size = 5,
           zsc.col = c("#FC8D59","#FFFFBF","#99D594")) + 
    theme(plot.title = element_text(hjust = 0.5))
}
p <- as.ggplot(p)

p
```

## Full Tutorial
https://openbiox.github.io/Bizard/Hiplot/079-gocircle.html
