# Skill: Piestats Group (R)

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
# Piestats Group
g <- unique(data[,axis[2]])
plist <- list()
for (i in 1:length(g)) {
  fil <- data[,axis[2]] == g[i]
  plist[[i]] <- 
    ggpiestats(
      data = data[fil,], x = genre, 
      title= paste('', axis[2], g[i], sep = ':'),
      plotgrid.args = list(ncol = 3),
      label.repel = TRUE,
      k = 2
    ) +
    scale_fill_manual(values = c("#3B4992FF","#EE0000FF","#008B45FF","#631879FF",
                                 "#008280FF","#BB0021FF","#5F559BFF","#A20056FF",
                                 "#808180FF"))
}

plot_grid(plotlist = plist, ncol = 3)
```

## Full tutorial
https://openbiox.github.io/Bizard/Hiplot/069-ggpiestats-group.html
