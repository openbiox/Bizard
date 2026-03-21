# Skill: PCA (R)

## Category
Hiplot

## When to Use
Principal component analysis (PCA) is a data processing method with "dimension reduction" as the core, replacing multi-index data with a few comprehensive indicators (PCA), and restoring the most essential characteristics of data.

## Required R Packages
- data.table
- ggplot2
- ggpubr
- gmodels
- jsonlite

## Minimal Reproducible Code
```r
# Load packages
library(data.table)
library(ggplot2)
library(ggpubr)
library(gmodels)
library(jsonlite)

# Prepare data
# Load data
data <- data.table::fread(jsonlite::read_json("https://hiplot.cn/ui/basic/pca/data.json")$exampleData[[1]]$textarea[[1]])
data <- as.data.frame(data)
group <- data.table::fread(jsonlite::read_json("https://hiplot.cn/ui/basic/pca/data.json")$exampleData[[1]]$textarea[[2]])
group <- as.data.frame(group)

# Convert data structure
rownames(data) <- data[, 1]
data <- as.matrix(data[, -1])
pca_info <- fast.prcomp(data)
## Create configuration
conf <- list(
  dataArg = list(
    list(list(value = "group")),  # Color by group
    list(list(value = ""))         # No shape group
  ),
  general = list(
    title = "Principal Component Analysis",
    palette = "Set1"
  )
)
## Perform PCA - Note: data must be transposed because PCA analyzes samples (columns)
pca_info <- prcomp(t(data), scale. = TRUE)
## Prepare plot data
axis <- sapply(conf$dataArg[[1]], function(x) x$value)
## Process color grouping
if (is.null(axis[1]) || axis[1] == "") {
  colorBy <- rep('ALL', ncol(data))
} else {
  ## Ensure sample order matches
  colorBy <- group[match(colnames(data), group$sample), axis[1]]
}
colorBy <- factor(colorBy, levels = unique(colorBy))
## Create PCA data frame
pca_data <- data.frame(
  sample = rownames(pca_info$x),
  PC1 = pca_info$x[, 1],
  PC2 = pca_info$x[, 2],
  colorBy = colorBy
)
## Calculate explained variance
variance_explained <- round(pca_info$sdev^2 / sum(pca_info$sdev^2) * 100, 1)
# ... (see full tutorial for more)
```

## Key Parameters
- `x`: Maps `PC1` to the x aesthetic
- `y`: Maps `PC2` to the y aesthetic
- `color`: Maps `colorBy` to the color aesthetic
- `alpha`: Controls transparency (0 = fully transparent, 1 = opaque)
- `width`: Controls element width
- `position`: Position adjustment (identity, dodge, stack, fill)
- `stat`: Statistical transformation to use

## Tips
- Use `theme_minimal()` or `theme_bw()` for clean, publication-ready plots
- Customize color scales with `scale_fill_manual()` or `scale_color_brewer()`
- See the full tutorial for additional customization options and advanced examples

## Full Tutorial
https://openbiox.github.io/Bizard/Hiplot/187-pca.html
