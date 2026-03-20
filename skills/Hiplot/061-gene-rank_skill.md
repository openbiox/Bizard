# Skill: Gene Ranking Dotplot (R)

## Category
Hiplot

## When to use
::: callout-note
**Hiplot website**

## Required R packages
- RColorBrewer
- data.table
- ggplot2
- ggrepel
- jsonlite

## Minimal reproducible code
```r
# Gene Ranking Dotplot
p <- 
  ggplot(data, aes(rank, log2FC, color = pvalue, size = abs(log2FC))) + 
  geom_point() + 
  scale_color_gradientn(colours = colorRampPalette(brewer.pal(11,'RdYlBu'))(100)) +
  geom_hline(yintercept = c(-1, 1), linetype = 2, size = 0.3) +
  geom_hline(yintercept = 0, linetype = 1, size = 0.5) +
  geom_vline(xintercept = median(data$rank), linetype = 2, size = 0.3) + 
  geom_text_repel(data = data2, aes(rank, log2FC, label = gene),
                  size = 3, color = "red") +
  xlab("") + ylab("") + 
  ylim(c(-max(abs(data$log2FC)), max(abs(data$log2FC)))) +
  labs(color = "Pvalue", size = "Log2FoldChange") +
  theme_bw(base_size = 12) +
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
https://openbiox.github.io/Bizard/Hiplot/061-gene-rank.html
