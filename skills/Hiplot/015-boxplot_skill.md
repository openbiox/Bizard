# Skill: Boxplot (R)

## Category
Hiplot

## When to use
::: callout-note
**Hiplot website**

## Required R packages
- data.table
- ggpubr
- ggthemes
- jsonlite

## Minimal reproducible code
```r
# Boxplot
p <- ggboxplot(data, x = "Group1", y = "Value", notch = F, facet.by = "Group2",
               add = "point", color = "Group1", xlab = "Group2", ylab = "Value",
               palette = c("#e04d39","#5bbad6","#1e9f86"),
               title = "Box Plot") +
  stat_compare_means(comparisons = my_comparisons, label = "p.format", 
                     method = "t.test") +
  scale_y_continuous(expand = expansion(mult = c(0.1, 0.1))) +
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

## Full tutorial
https://openbiox.github.io/Bizard/Hiplot/015-boxplot.html
