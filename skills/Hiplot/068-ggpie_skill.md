# Skill: GGPIE (R)

## Category
Hiplot

## When to use
::: callout-note
**Hiplot website**

## Required R packages
- cowplot
- data.table
- dplyr
- ggpie
- ggplot2
- jsonlite

## Minimal reproducible code
```r
# GGPIE
plist <- list()
for (j in unique(data[, axis[2]])) {
  plist[[j]] <- ggpie(
    data = data[data[, axis[2]] == j,],
    group_key = axis[1], count_type = "full",
    label_type = "horizon", label_size = 8,
    label_info = "all", label_pos = "out") + 
    scale_fill_manual(values = c("#00468BFF","#ED0000FF")) +
    ggtitle(j)
  }

plot_grid(plotlist = plist, ncol = 3)
```

## Full tutorial
https://openbiox.github.io/Bizard/Hiplot/068-ggpie.html
