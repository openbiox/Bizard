# Skill: Density-Histogram (R)

## Category
Hiplot

## When to use
::: callout-note
**Hiplot website**

## Required R packages
- data.table
- dplyr
- grafify
- jsonlite

## Minimal reproducible code
```r
# Density Plot
p <- plot_density(
  data = data, 
  ycol = get(y), 
  group = get(group),
  linethick = 0.5,
  c_alpha = 0.6) + 
  ggtitle("Density Plot") + 
  geom_vline(aes_string(xintercept = "median"),
        colour = 'black', linetype = 2, size = 0.5) + 
  xlab(y) + 
  ylab("density") + 
  guides(fill = guide_legend(title = group), color = FALSE) +
  theme(text = element_text(family = "Arial"),
        plot.title = element_text(size = 12,hjust = 0.5),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 10),
        axis.text.x = element_text(angle = 0, hjust = 0.5,vjust = 1),
        legend.position = "bottom",
        legend.direction = "horizontal",
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 10))

p
```

## Full tutorial
https://openbiox.github.io/Bizard/Hiplot/039-density-histogram.html
