# Skill: Barstats (R)

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
# Barstats
g <- unique(data[,axis[3]])
plist <- list()
for (i in 1:length(g)) {
  fil <- data[,axis[3]] == g[i]
  plist[[i]] <- ggbarstats(
    data = data[fil,], x = relig, y = partyid,
    plotgrid.args = list(ncol = 1), paired = F, k = 2) +
    scale_fill_manual(values = c("#00468BFF","#ED0000FF","#42B540FF"))
}
p <- plot_grid(plotlist = plist, ncol = 1)

p
```

## Full tutorial
https://openbiox.github.io/Bizard/Hiplot/063-ggbarstats.html
