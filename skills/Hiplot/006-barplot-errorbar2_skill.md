# Skill: Barplot (errorbar2) (R)

## Category
Hiplot

## When to use
::: callout-note
**Hiplot website**

## Required R packages
- data.table
- ggplot2
- ggpubr
- grafify
- jsonlite

## Minimal reproducible code
```r
# Barplot (errorbar2)
p <- plot_scatterbar_sd(
  data, ycol = get(colnames(data)[1]), xcol = get(colnames(data)[2]),
  b_alpha = 1, ewid = 0.2, jitter = 0.1) +
  stat_compare_means(data = data, aes(data[, 2], data[, 1], fill = data[, 2]),
                     label = "p.format", ref.group = ".all.", vjust = -2, 
                     method = "t.test") +
  guides(fill=guide_legend(title=colnames(data)[2])) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.2))) +
  labs(x="class", y="score") +
  scale_fill_manual(values = c("#E64B35FF","#4DBBD5FF","#00A087FF","#3C5488FF")) +
  theme_classic2() +
  theme(text = element_text(family = "Arial"),
        plot.title = element_text(size = 12,hjust = 0.5),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 10),
        axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 1),
        legend.position = "right",
        legend.direction = "vertical",
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 10))

p
```

## Full tutorial
https://openbiox.github.io/Bizard/Hiplot/006-barplot-errorbar2.html
