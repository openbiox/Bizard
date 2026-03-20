# Skill: Barcode Plot (R)

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
# Barcode Plot
p <- ggplot(data, aes(x = sales, y = region)) +
  geom_tile(width = 0.01, height = 0.9, fill = "#606fcc") + # Control the width and height of the Barcode
  theme_bw() +
  labs(title = "Sales report", x = "Sales", y = "Region") +
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
https://openbiox.github.io/Bizard/Hiplot/002-barcode-plot.html
