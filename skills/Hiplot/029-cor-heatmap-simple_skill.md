# Skill: Simplified Correlation Heatmap (R)

## Category
Hiplot

## When to use
::: callout-note
**Hiplot website**

## Required R packages
- data.table
- ggplot2
- jsonlite
- sigminer

## Minimal reproducible code
```r
# Simplified Correlation Heatmap
p <- show_cor(
  data = data,
  x_vars = c("mpg","cyl","disp"),
  y_vars = c("wt","hp","drat"),
  cor_method = "pearson",
  vis_method = "square",
  lab = T,
  test = T,
  hc_order = F,
  legend.title = "Corr"
  ) +
  ggtitle("") +
  labs(x="", y="") +
  theme_bw() +
  theme(text = element_text(family = "Arial"),
        plot.title = element_text(size = 12,hjust = 0.5),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 10),
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
        legend.position = "right",
        legend.direction = "vertical",
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 10))

p
```

## Full tutorial
https://openbiox.github.io/Bizard/Hiplot/029-cor-heatmap-simple.html
