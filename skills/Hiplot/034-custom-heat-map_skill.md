# Skill: Custom Heatmap (R)

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
# Custom Heatmap
p <- ggplot(df, aes(x = col, y = row, fill = value)) +
  geom_point(shape = 21, size = 8, aes(fill = value), color = "white") +
  scale_fill_gradient(low = "#DDDDDD", high = "#0000F5") +
  guides(fill = guide_colorbar(title = "Value")) +
  theme(
    panel.background = element_rect(fill = "white"),
    panel.grid = element_blank(),
    axis.text = element_text(size = 10),
    axis.ticks = element_blank(),
    axis.title = element_blank()
    ) +
  scale_x_continuous(breaks = 1:col_num, labels = col_labels, position = "top") +
  scale_y_reverse(breaks = 1:row_num, labels = row_labels, position = "left")

p
```

## Full tutorial
https://openbiox.github.io/Bizard/Hiplot/034-custom-heat-map.html
