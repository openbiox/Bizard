# Skill: Waffle Plot (R)

## Category
Hiplot

## When to use
::: callout-note
**Hiplot website**

## Required R packages
- data.table
- jsonlite
- waffle

## Minimal reproducible code
```r
# Waffle Plot
p <- waffle(parts, rows = 8, size = 1, legend_pos = "right") +
  ggtitle("Waffle Plot") +
  scale_fill_manual(values = c("#e04d39","#5bbad6","#1e9f86")) +
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
https://openbiox.github.io/Bizard/Hiplot/184-waffle.html
