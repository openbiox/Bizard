# Skill: PCA2 (R)

## Category
Hiplot

## When to Use
Principal component analysis (PCA) is a data processing method with "dimension reduction" as the core, replacing multi-index data with a few comprehensive indicators (PCA), and restoring the most essential characteristics of data.

## Required R Packages
- FactoMineR
- data.table
- factoextra
- jsonlite

## Minimal Reproducible Code
```r
# Load packages
library(FactoMineR)
library(data.table)
library(factoextra)
library(jsonlite)

# Prepare data
# Load data
data <- data.table::fread(jsonlite::read_json("https://hiplot.cn/ui/basic/pca2/data.json")$exampleData[[1]]$textarea[[1]])
data <- as.data.frame(data)
sample_info <- data.table::fread(jsonlite::read_json("https://hiplot.cn/ui/basic/pca2/data.json")$exampleData[[1]]$textarea[[2]])
sample_info <- as.data.frame(sample_info)

# Convert data structure
row.names(sample_info) <- sample_info[,1]
sample_info <- sample_info[colnames(data)[-1],]
## tsne
rownames(data) <- data[, 1]
data <- as.matrix(data[, -1])
pca_data <- PCA(t(as.matrix(data)), scale.unit = TRUE, ncp = 5, graph = FALSE)

# View data
head(data[,1:5])

# Create visualization
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

## Key Parameters
- `position`: Position adjustment (identity, dodge, stack, fill)
- `theme`: Plot theme; tutorial uses `theme_bw()`
- `fill`: Maps a variable to fill color for group comparison
- `color`: Maps a variable to outline/point color

## Tips
- Use `theme_minimal()` or `theme_bw()` for clean, publication-ready plots
- Customize color scales with `scale_fill_manual()` or `scale_color_brewer()`
- See the full tutorial for additional customization options and advanced examples

## Full Tutorial
https://openbiox.github.io/Bizard/Hiplot/135-pca2.html
