# Skill: GOBubble Plot (R)

## Category
Hiplot

## When to use
::: callout-note
**Hiplot website**

## Required R packages
- GOplot
- data.table
- ggplotify
- jsonlite

## Minimal reproducible code
```r
# GOBubble Plot
p <- function () {
  GOBubble(data, display = "single", title = "GO Enrichment Bubbleplot",
           colour = c("#FC8D59","#FFFFBF","#99D594"),
           labels = 0,  ID = T, table.legend = T, table.col = T, bg.col = F) + 
  theme(plot.title = element_text(hjust = 0.5))
}
p <- as.ggplot(p)

p
```

## Full tutorial
https://openbiox.github.io/Bizard/Hiplot/078-gobubble.html
