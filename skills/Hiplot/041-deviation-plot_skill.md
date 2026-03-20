# Skill: Deviation Plot (R)

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
# Deviation Plot
p <- ggbarplot(data,
    x = "name",
    y = "z_score",
    fill = "Group",
    color = "white",
    sort.val = "desc",
    sort.by.groups = FALSE,
    x.text.angle = 90,
    xlab = "name",
    ylab = "mpg",
    rotate = TRUE
  ) +
  scale_fill_manual(values = c("#e04d39","#5bbad6")) +
  theme_bw() +
  theme(text = element_text(family = "Arial"),
        plot.title = element_text(size = 12,hjust = 0.5),
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
https://openbiox.github.io/Bizard/Hiplot/041-deviation-plot.html
