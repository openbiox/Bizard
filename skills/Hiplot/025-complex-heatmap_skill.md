# Skill: Complex Heatmap (R)

## Category
Hiplot

## When to Use
A multi-omics plugins to draw heatmap, meta annotation, and mutations.

## Required R Packages
- ComplexHeatmap
- circlize
- cowplot
- data.table
- ggplotify
- hiplotlib
- jsonlite
- randomcoloR
- stringr

## Minimal Reproducible Code
```r
# Load packages
library(ComplexHeatmap)
library(circlize)
library(cowplot)
library(data.table)
library(ggplotify)
library(hiplotlib)

# Prepare data
# Load data
data <- data.table::fread(jsonlite::read_json("https://hiplot.cn/ui/basic/complex-heatmap/data.json")$exampleData[[1]]$textarea[[1]])
data <- as.data.frame(data)
data2 <- data.table::fread(jsonlite::read_json("https://hiplot.cn/ui/basic/complex-heatmap/data.json")$exampleData[[1]]$textarea[[2]])
data2 <- as.data.frame(data2)

# convert data structure
keep_vars_ref <- ls() 
row.names(data) <- data[, 1]
data <- data[, -1]
axis_raw <- c("KRAS","GBP4")
exp_start_col <- which(colnames(data) == axis_raw[2])
mut_start_col <- which(colnames(data) == axis_raw[1])
heat_mat <- as.matrix(t(data[, exp_start_col:ncol(data)]))
mut_mat <- as.matrix(t(data[, mut_start_col:(exp_start_col - 1)]))
mut_mat[is.na(mut_mat)] <- ""

color_key <- c("#196ABD", "#3399FF", "#3399FF", "#f4f4f4", "#f4f4f4", "#f4f4f4", "#FF3333", "#FF3333", "#C20B01")

cols <- c()
for (i in 1:nrow(data2)) {
  cols[data2[i,1]] <- data2[i,2]
}
col_meta <- list()
col_meta_pre <- list()
items <- c()
for (i in 1:(mut_start_col - 1)) {
  ref <- unique(data[, i])
  ref <- ref[!is.na(ref) & ref != ""]
  if (any(is.numeric(ref)) & length(ref) > 2) {
    col_meta_pre[[colnames(data)[i]]] <- hiplotlib::col_fun_cont(data[,i])
  } else if (length(ref) == 2 & any(is.numeric(ref))) {
    col_meta_pre[[colnames(data)[i]]] <- c("#f4f4f4", "#5a5a5a")
    items <- c(items, ref)
  } else if (length(ref) == 2 & any(is.character(ref))) {
    col_meta_pre[[colnames(data)[i]]] <- c("#196ABD", "#C20B01")
    items <- c(items, unique(data[, i]))
  } else if (length(unique(data[, i])) > 2) {
    col_meta_pre[[colnames(data)[i]]] <- distinctColorPalette(
      length(unique(data[, i]))
    )
# ... (see full tutorial for more)
```

## Key Parameters
- `width`: Controls element width
- `fill`: Maps a variable to fill color for group comparison
- `color`: Maps a variable to outline/point color

## Tips
- Adjust text size with `theme(text = element_text(size = 14))` for presentations
- See the full tutorial for additional customization options and advanced examples

## Full Tutorial
https://openbiox.github.io/Bizard/Hiplot/025-complex-heatmap.html
