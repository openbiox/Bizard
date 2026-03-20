# Skill: Correlation Heatmap (R)

## Category
Hiplot

## When to use
::: callout-note
**Hiplot website**

## Required R packages
- data.table
- ggcorrplot
- jsonlite

## Minimal reproducible code
```r
# Correlation Heatmap
p <- ggcorrplot(
  corr,
  colors = c("#4477AA", "#FFFFFF", "#BB4444"),
  method = "circle",
  hc.order = T,
  hc.method = "ward.D2",
  outline.col = "white",
  ggtheme = theme_bw(),
  type = "upper",
  lab = F,
  lab_size = 3,
  legend.title = "Correlation"
  ) +
  ggtitle("Cor Heatmap Plot") +
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
https://openbiox.github.io/Bizard/Hiplot/030-cor-heatmap.html
