# Skill: Line (errorbar) (R)

## Category
Hiplot

## When to use
::: callout-note
**Hiplot website**

## Required R packages
- data.table
- ggpubr
- jsonlite

## Minimal reproducible code
```r
# Line (errorbar)
p <- ggline(
  data, x = "Group1", y = "Value", color = "Group2",
  add = "mean_se", title = "Line plot with errorbar", palette = "npg") +
  stat_compare_means(aes_(group = as.name("Group2"))) +
  theme_bw() +
  theme(text = element_text(family = "Arial"),
        plot.title = element_text(size = 12, hjust = 0.5),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 10),
        axis.text.x = element_text(angle = 0, hjust = 0.5,vjust = 1),
        legend.position = "right",
        legend.direction = "vertical",
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 10))

p
```

## Full tutorial
https://openbiox.github.io/Bizard/Hiplot/093-line-errorbar.html
