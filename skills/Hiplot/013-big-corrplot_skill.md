# Skill: Corrplot Big Data (R)

## Category
Hiplot

## When to Use
The correlation heat map is a graph that analyzes the correlation between two or more variables.

## Required R Packages
- ComplexHeatmap
- data.table
- jsonlite

## Minimal Reproducible Code
```r
# Load packages
library(ComplexHeatmap)
library(data.table)
library(jsonlite)

# Prepare data
# Load data
data <- data.table::fread(jsonlite::read_json("https://hiplot.cn/ui/basic/big-corrplot/data.json")$exampleData$textarea[[1]])
data <- as.data.frame(data)

# convert data structure
data <- data[!is.na(data[, 1]), ]
idx <- duplicated(data[, 1])
data[idx, 1] <- paste0(data[idx, 1], "--dup-", cumsum(idx)[idx])
rownames(data) <- data[, 1]
data <- data[, -1]
str2num_df <- function(x) {
  x[] <- lapply(x, function(l) as.numeric(l))
  x
}
tmp <- t(str2num_df(data))
corr <- round(cor(tmp, use = "na.or.complete", method = "pearson"), 3)

# View data
head(corr[,1:5])

# Create visualization
# Corrplot Big Data
p <- ComplexHeatmap::Heatmap(
  corr, col = colorRampPalette(c("#4477AA","#FFFFFF","#BB4444"))(50),
  clustering_distance_rows = "euclidean",
  clustering_method_rows = "ward.D2",
  clustering_distance_columns = "euclidean",
  clustering_method_columns = "ward.D2",
  show_column_dend = FALSE, show_row_dend = FALSE,
  column_names_gp = gpar(fontsize = 8),
  row_names_gp = gpar(fontsize = 8)
)

p
```

## Key Parameters
- `fill`: Maps a variable to fill color for group comparison
- `color`: Maps a variable to outline/point color

## Tips
- Adjust text size with `theme(text = element_text(size = 14))` for presentations
- See the full tutorial for additional customization options and advanced examples

## Full Tutorial
https://openbiox.github.io/Bizard/Hiplot/013-big-corrplot.html
