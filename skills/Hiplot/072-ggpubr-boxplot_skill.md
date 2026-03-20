# Skill: GGPubr Boxplot (R)

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
# GGPubr Boxplot
p <- ggboxplot(
  data = data, x = "supp", y = "len", facet.by = "dose",
  merge = T,
  color = "supp",
  fill = "white") + 
  stat_compare_means(
    label = "p.signif",
    label.x.npc = "center",
    method = "wilcox") + 
  scale_y_continuous(expand = expansion(mult = c(0.2, 0.2))) +
  scale_fill_manual(values = c("#e04d39","#5bbad6")) +
  ggtitle("Complex Boxplot") + 
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
https://openbiox.github.io/Bizard/Hiplot/072-ggpubr-boxplot.html
