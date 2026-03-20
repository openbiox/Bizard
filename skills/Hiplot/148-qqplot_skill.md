# Skill: QQ Plot (R)

## Category
Hiplot

## When to use
::: callout-note
**Hiplot website**

## Required R packages
- data.table
- grafify
- jsonlite

## Minimal reproducible code
```r
# QQ Plot
p <- plot_qqline(data = data, ycol = Cytokine, group = Genotype,
                 symsize = 2, symthick = 0.5, s_alpha = 1) +
  ggtitle("QQplot without facet") +
  xlab("theoretical") + ylab("sample") + 
  guides(fill = guide_legend(title = "Genotype")) +
  scale_color_manual(values = c("#E69F00","#4DB1DC")) +
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
https://openbiox.github.io/Bizard/Hiplot/148-qqplot.html
