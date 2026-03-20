# Skill: tSNE (R)

## Category
Hiplot

## When to use
::: callout-note
**Hiplot website**

## Required R packages
- Rtsne
- data.table
- ggpubr
- jsonlite

## Minimal reproducible code
```r
# tsne
p <- ggscatter(data = tsne_data, x = "tSNE_1", y = "tSNE_2", size = 2, 
               palette = "lancet", color = "colorBy") +
  labs(color = "group") +
  ggtitle("tSNE Plot1") +
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
https://openbiox.github.io/Bizard/Hiplot/175-tsne.html
