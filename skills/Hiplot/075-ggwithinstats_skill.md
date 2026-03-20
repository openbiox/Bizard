# Skill: Complex-Violin (R)

## Category
Hiplot

## When to use
::: callout-note
**Hiplot website**

## Required R packages
- cowplot
- data.table
- ggplot2
- ggstatsplot
- jsonlite

## Minimal reproducible code
```r
# Complex-Violin
g <- unique(data[,axis[3]])
plist <- list()
for (i in 1:length(g)) {
  fil <- data[,axis[3]] == g[i]
  plist[[i]] <- ggwithinstats(
    data = data[fil,], x = condition, y = desire,
    title= paste('', axis[3], g[i], sep = ':'),
    p.adjust.method = "holm",
    plot.type = "boxviolin",
    pairwise.comparisons = T,
    pairwise.display = "significant",
    effsize.type = "unbiased",
    notch = T,
    type = "parametric",
    k = 2,
    plotgrid.args = list(ncol = 2)
  ) +
    scale_color_manual(values = c("#3B4992FF","#EE0000FF"))
}

plot_grid(plotlist = plist, ncol = 2)
```

## Full tutorial
https://openbiox.github.io/Bizard/Hiplot/075-ggwithinstats.html
