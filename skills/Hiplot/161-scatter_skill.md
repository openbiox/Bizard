# Skill: Scatter (R)

## Category
Hiplot

## When to use
::: callout-note
**Hiplot website**

## Required R packages
- data.table
- ggplot2
- jsonlite

## Minimal reproducible code
```r
# Scatter
p <- ggplot(data, aes(x = Value1, y = Value2)) +
  geom_point(size = 1, alpha = 1, aes(color = Group, shape = Group)) +
  ggtitle("Scatter Plot") +
  scale_color_manual(values = c("#00468BFF", "#ED0000FF")) +
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
https://openbiox.github.io/Bizard/Hiplot/161-scatter.html
