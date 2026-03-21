# Skill: Stack Violin (R)

## Category
Hiplot

## When to Use
The expression of key genes in each cluster in single-cell transcriptomic (Single Cell RNA-Seq)analysis.

## Required R Packages
- Seurat
- ggplot2
- limma
- readr

## Minimal Reproducible Code
```r
# Load packages
library(Seurat)
library(ggplot2)
library(limma)
library(readr)

# Prepare data
# Load data
data <- readr::read_delim("https://download.hiplot.cn/api/file/fetch/?path=/c622f9b0-54da-11f0-ba5f-8dc116702904/public/demo/stack-violin.txt")

# convert data structure
data <- as.matrix(data)
rownames(data) <- data[, 1]
exp <- data[, 2:ncol(data)]
dimnames <- list(
  rownames(exp),
  colnames(exp)
)
data <- matrix(as.numeric(as.matrix(exp)),
  nrow = nrow(exp),
  dimnames = dimnames
)
data <- avereps(data,
  ID = rownames(data)
)
## Convert the matrix to a Seurat object and filter the data
pbmc <- CreateSeuratObject(
  counts = data,
  project = "seurat",
  min.cells = 0,
  min.features = 0,
  names.delim = "_",
)
## Calculate the percentage of mitochondrial genes using the PercentageFeatureSet function
pbmc[["percent.mt"]] <- PercentageFeatureSet(
  object = pbmc,
  pattern = "^MT-"
)
## Filter the data
pbmc <- subset(
  x = pbmc,
  subset = nFeature_RNA > 50 & percent.mt < 5
)
## Normalize the data
pbmc <- NormalizeData(
  object = pbmc,
  normalization.method = "LogNormalize",
  scale.factor = 10000, verbose = F
)
## Extract genes with large coefficient of variation between cells
# ... (see full tutorial for more)
```

## Key Parameters
- `position`: Position adjustment (identity, dodge, stack, fill)
- `fill`: Maps a variable to fill color for group comparison
- `color`: Maps a variable to outline/point color

## Tips
- Customize color scales with `scale_fill_manual()` or `scale_color_brewer()`
- Adjust text size with `theme(text = element_text(size = 14))` for presentations
- See the full tutorial for additional customization options and advanced examples

## Full Tutorial
https://openbiox.github.io/Bizard/Hiplot/166-stack-violin.html
