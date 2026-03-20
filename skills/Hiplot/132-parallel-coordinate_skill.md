# Skill: Parallel Coordinate (R)

## Category
Hiplot

## When to use
::: callout-note
**Hiplot website**

## Required R packages
- GGally
- data.table
- ggthemes
- hrbrthemes
- jsonlite
- viridis

## Minimal reproducible code
```r
# Parallel Coordinate
p <- ggparcoord(data, columns = 2:(ncol(data) - 1), groupColumn = ncol(data),
                title = "Parallel Coordinate Plot for cancer Data",
                alphaLines = 0.3, scale = "globalminmax",
                showPoints = T, boxplot = F) +
  theme_base() +
  theme(text = element_text(family = "Arial"),
        plot.title = element_text(size = 12, hjust = 0.5),
        axis.title = element_text(size = 10),
        axis.text = element_text(size = 12),
        axis.text.x = element_text(angle = 0, hjust = 0.5,vjust = 1),
        legend.position = "right",
        legend.direction = "vertical",
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 10)) +
  scale_color_viridis(discrete = TRUE) +
  facet_grid(formula(paste("~", (colnames(data)[ncol(data)]))))

p
```

## Full tutorial
https://openbiox.github.io/Bizard/Hiplot/132-parallel-coordinate.html
