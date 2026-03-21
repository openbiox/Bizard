# Skill: Violin (R)

## Category
Hiplot

## When to Use
::: callout-note
**Hiplot website**

## Required R Packages
- data.table
- ggpubr
- ggthemes
- jsonlite

## Minimal Reproducible Code
```r
# Violin
p <- ggviolin(data, x = "Tumor", y = "Expresssion", fill = "Tumor", add = "boxplot",
              xlab = "Tumor", ylab = "Expresssion", 
              add.params = list(fill = "white"),
              palette = c("#e04d39","#5bbad6","#1e9f86"),
              title = "Violin Plot", alpha = 1) + 
  stat_compare_means(comparisons = my_comparisons, label = "p.signif") +
  theme_stata() +
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

## Full Tutorial
https://openbiox.github.io/Bizard/Hiplot/181-violin.html
