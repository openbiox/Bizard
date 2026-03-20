# Skill: Contour (XY) (R)

## Category
Hiplot

## When to use
::: callout-note
**Hiplot website**

## Required R packages
- data.table
- ggisoband
- ggplot2
- jsonlite

## Minimal reproducible code
```r
# Contour (XY)
p <- ggplot(data, aes(xvalue, yvalue)) +
  geom_density_bands(
    alpha = 1,
    aes(fill = stat(density)), color = "gray40", size = 0.2
    ) +
  geom_point(alpha = 1, shape = 21, fill = "white") +
  scale_fill_viridis_c(guide = "legend") +
  ylab("value2") +
  xlab("value1") +
  ggtitle("Contour-XY Plot") +
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
https://openbiox.github.io/Bizard/Hiplot/028-contour-xy.html
