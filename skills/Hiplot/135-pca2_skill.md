# Skill: PCA2 (R)

## Category
Hiplot

## When to use
::: callout-note
**Hiplot website**

## Required R packages
- FactoMineR
- data.table
- factoextra
- jsonlite

## Minimal reproducible code
```r
# PCA2
p <- fviz_pca_ind(pca_data, geom.ind = "point", pointsize = 6, addEllipses = TRUE,
                  mean.point = F, col.ind = sample_info[,"Group"]) +
  ggtitle("Principal Component Analysis") +
  scale_fill_manual(values = c("#00468BFF","#ED0000FF")) +
  scale_color_manual(values = c("#00468BFF","#ED0000FF")) +
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
https://openbiox.github.io/Bizard/Hiplot/135-pca2.html
