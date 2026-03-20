# Skill: Betweenstats (R)

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
# Betweenstats
g <- unique(data[,axis[3]])
plist <- list()
for (i in 1:length(g)) {
  fil <- data[,axis[3]] == g[i]
  plist[[i]] <- ggbetweenstats(
    data = data[fil,], x = mpaa, y = length,
    title= paste('', axis[3], g[i], sep = ':'),
    p.adjust.method = "holm",
    plot.type = "boxviolin",
    pairwise.comparisons = T,
    pairwise.display = "significant",
    effsize.type = "unbiased",
    notch = T,
    type = "parametric",
    plotgrid.args = list(ncol = 2)) +
    scale_color_manual(values = c("#00468BFF","#ED0000FF","#42B540FF"))
}
p <- plot_grid(plotlist = plist, ncol = 2)

p
```

## Full tutorial
https://openbiox.github.io/Bizard/Hiplot/064-ggbetweenstats.html
