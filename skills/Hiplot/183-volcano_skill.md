# Skill: Volcano (R)

## Category
Hiplot

## When to use
::: callout-note
**Hiplot website**

## Required R packages
- data.table
- ggpubr
- jsonlite

## Minimal reproducible code
```r
# Volcano
options(ggrepel.max.overlaps = 100)
p <- ggscatter(data, x = "logFC", y = "logP", color = "Group", 
               palette = c("#2f5688", "#BBBBBB", "#CC0000"), size = 1, 
               alpha = 0.5, font.label = 8, repel = TRUE, label=data$Label,
               xlab = "log2(Fold Change)", ylab = "-log10(P Value)",
               show.legend.text = FALSE) +
  ggtitle("Volcano Plot") +
  geom_hline(yintercept = -log(0.05, 10), linetype = "dashed") +
  geom_vline(xintercept = c(2, -2), linetype = "dashed") +
  theme_bw() +
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
https://openbiox.github.io/Bizard/Hiplot/183-volcano.html
