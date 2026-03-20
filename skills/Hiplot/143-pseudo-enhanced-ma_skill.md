# Skill: EnhancedMA (R)

## Category
Hiplot

## When to use
::: callout-note
**Hiplot website**

## Required R packages
- EnhancedVolcano
- data.table
- jsonlite

## Minimal reproducible code
```r
# EnhancedMA
p <- EnhancedVolcano(
  data, lab = rownames(data), title = "MA plot", subtitle = "EnhancedMA",
  x = 'log2FoldChange', y = 'baseMeanNew', xlab = bquote(~Log[2]~ 'fold change'),
  ylab = bquote(~Log[e]~ 'base mean + 1'), ylim = c(0,12),
  pCutoff = as.numeric(1e-05), FCcutoff = 1, pointSize = 3.5,
  labSize = 4, boxedLabels = T, colAlpha = 1,
  legendLabels = c('NS', expression(Log[2]~FC),
                   'Mean expression', 
                   expression(Mean-expression~and~log[2]~FC)),
  legendPosition = "bottom", legendLabSize = 16, legendIconSize = 4.0,
  encircleCol = 'black', encircleSize = 2.5, encircleFill = 'pink',
  encircleAlpha = 1/2) + 
  coord_flip() +
  theme(text = element_text(family = "Arial"),
        plot.title = element_text(size = 12,hjust = 0.5),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 10),
        axis.text.x = element_text(angle = 0, hjust = 0.5,vjust = 1),
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 10))

p
```

## Full tutorial
https://openbiox.github.io/Bizard/Hiplot/143-pseudo-enhanced-ma.html
