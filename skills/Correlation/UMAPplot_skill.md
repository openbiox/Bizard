# Skill: UMAP Plot (R)

## Category
Correlation

## When to Use
Create a UMAP Plot visualization in R for biomedical data analysis and research publications.

## Required R Packages
- RColorBrewer
- Seurat
- SeuratData
- dplyr
- ggplot2
- mlbench
- patchwork
- umap

## Minimal Reproducible Code
```r
# Load packages
library(RColorBrewer)
library(Seurat)
library(SeuratData)
library(dplyr)
library(ggplot2)
library(mlbench)

# Prepare data
data(BreastCancer)
wdbc_data <- BreastCancer[, -1]  # Remove the ID column
wdbc_data <- na.omit(wdbc_data)
features <- wdbc_data[, 1:9]  # Using the first 9 features
features <- as.data.frame(lapply(features, function(x) as.numeric(as.character(x))))
diagnosis <- wdbc_data$Class
head(features)

# Create visualization
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

## Key Parameters
- `color`: Maps `CellType` to the color aesthetic
- `alpha`: Controls transparency (0 = fully transparent, 1 = opaque)
- `stat`: Statistical transformation to use
- `theme`: Plot theme; tutorial uses `theme_minimal()`

## Tips
- Use `theme_minimal()` or `theme_bw()` for clean, publication-ready plots
- Customize color scales with `scale_fill_manual()` or `scale_color_brewer()`
- Always check and report the correlation coefficient and p-value alongside visual patterns

## Full Tutorial
https://openbiox.github.io/Bizard/Correlation/UMAPplot.html
