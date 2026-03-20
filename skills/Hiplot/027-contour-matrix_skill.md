# Skill: Contour (Matrix) (R)

## Category
Hiplot

## When to use
::: callout-note
**Hiplot website**

## Required R packages
- cowplot
- data.table
- ggisoband
- ggplot2
- jsonlite
- reshape2

## Minimal reproducible code
```r
# Contour (Matrix)
complex_general_theme <- 
  theme(text = element_text(family = "Arial"),
        plot.title = element_text(size = 12,hjust = 0.5),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 10),
        axis.text.x = element_text(angle = 0, hjust = 0.5,vjust = 1),
        legend.position = "right",
        legend.direction = "vertical",
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 10))

p1 <- ggplot(data3d, aes(x, y, z = z)) +
  geom_isobands(
    alpha = 1,
    aes(color = stat(zmin)), fill = NA
  ) +
  scale_color_viridis_c() +
  coord_cartesian(expand = FALSE) +
  theme_bw() +
  complex_general_theme

p2 <- ggplot(data3d, aes(x, y, z = z)) +
  geom_isobands(
    alpha = 1,
    aes(fill = stat(zmin)), color = NA
  ) +
  scale_fill_viridis_c(guide = "legend") +
  coord_cartesian(expand = FALSE) +
  theme_bw() +
  complex_general_theme

plot_grid(p1, p2, labels = c("A", "B"), label_size = 12)
```

## Full tutorial
https://openbiox.github.io/Bizard/Hiplot/027-contour-matrix.html
