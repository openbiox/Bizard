# Skill: UMAP Plot (R)

## Category
Correlation

## When to use
Visualize umap plot data in a biomedical context.

## Required R packages
- RColorBrewer
- Seurat
- SeuratData
- dplyr
- ggplot2
- mlbench
- patchwork
- umap

## Minimal reproducible code
```r
set.seed(123)
wdbc_umap <- umap(features, 
                 n_neighbors = 15, 
                 min_dist = 0.2,
                 metric = "euclidean")

ggplot(data.frame(wdbc_umap$layout, Diagnosis = diagnosis),
       aes(X1, X2, color = Diagnosis)) +
  geom_point(size = 3, alpha = 0.8) +
  stat_ellipse(level = 0.9) +
  theme_minimal() +
  labs(title = "UMAP of Wisconsin Breast Cancer Dataset",
       x = "UMAP1", y = "UMAP2",
       subtitle = "n_neighbors=15, min_dist=0.2") +
  scale_color_manual(values = c("benign" = "#1b9e77", "malignant" = "#d95f02"))
```

## Full tutorial
https://openbiox.github.io/Bizard/Correlation/UMAPplot.html
