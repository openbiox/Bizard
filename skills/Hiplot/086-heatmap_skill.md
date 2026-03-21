# Skill: Heatmap (R)

## Category
Hiplot

## When to Use
Heat map is an intuitive and visual method for analyzing the distribution of experimental data, which can be used for quality control of experimental data and visualization display of difference data, as well as clustering of data and samples to observe sample quality.

## Required R Packages
- ComplexHeatmap
- data.table
- genefilter
- jsonlite

## Minimal Reproducible Code
```r
# Load packages
library(ComplexHeatmap)
library(data.table)
library(genefilter)
library(jsonlite)

# Prepare data
# Load data
data_count <- data.table::fread(jsonlite::read_json("https://hiplot.cn/ui/basic/heatmap/data.json")$exampleData[[1]]$textarea[[1]])
data_count <- as.data.frame(data_count)
data_sample <- data.table::fread(jsonlite::read_json("https://hiplot.cn/ui/basic/heatmap/data.json")$exampleData[[1]]$textarea[[2]])
data_sample <- as.data.frame(data_sample)
data_gene <- data.table::fread(jsonlite::read_json("https://hiplot.cn/ui/basic/heatmap/data.json")$exampleData[[1]]$textarea[[3]])
data_gene <- as.data.frame(data_gene)

# Convert data structure
data_count <- data_count[!is.na(data_count[, 1]), ]
idx <- duplicated(data_count[, 1])
data_count[idx, 1] <- paste0(data_count[idx, 1], "--dup-", cumsum(idx)[idx])
for (i in 2:ncol(data_count)) {
  data_count[, i] <- as.numeric(data_count[, i])
}
data <- as.matrix(data_count[, -1])
rownames(data) <- data_count[, 1]

## Add annotation information to samples
sample.info <- data_sample[-1]
row.names(sample.info) <- data_sample[, 1]
sample_info_reorder <- as.data.frame(sample.info[match(
  colnames(data), rownames(sample.info)
  ), ])
colnames(sample_info_reorder) <- colnames(sample.info)
rownames(sample_info_reorder) <- colnames(data)

## Add annotation information to genes
gene_info <- data_gene[-1]
rownames(gene_info) <- data_gene[, 1]
gene_info_reorder <- as.data.frame(gene_info[match(
  rownames(data), rownames(gene_info)
  ), ])
colnames(gene_info_reorder) <- colnames(gene_info)
rownames(gene_info_reorder) <- rownames(data)

# View data
head(data)

# Create visualization
# Heatmap
## Set annotation_col and annotation_row to add annotations to samples and genes respectively
top_var <- 100
# ... (see full tutorial for more)
```

## Key Parameters
- `fill`: Maps a variable to fill color for group comparison
- `color`: Maps a variable to outline/point color

## Tips
- Adjust text size with `theme(text = element_text(size = 14))` for presentations
- See the full tutorial for additional customization options and advanced examples

## Full Tutorial
https://openbiox.github.io/Bizard/Hiplot/086-heatmap.html
