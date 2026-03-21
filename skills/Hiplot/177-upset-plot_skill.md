# Skill: Upset Plot (R)

## Category
Hiplot

## When to Use
Upset can be used to show the interactive relationship between collections.

## Required R Packages
- ComplexHeatmap
- VennDiagram
- data.table
- ggplot2
- ggplotify
- jsonlite

## Minimal Reproducible Code
```r
# Load packages
library(ComplexHeatmap)
library(VennDiagram)
library(data.table)
library(ggplot2)
library(ggplotify)
library(jsonlite)

# Prepare data
# Load data
data <- data.table::fread(jsonlite::read_json("https://hiplot.cn/ui/basic/upset-plot/data.json")$exampleData[[1]]$textarea[[1]])
data <- as.data.frame(data)

# convert data structure
for (i in seq_len(ncol(data))) {
  data[is.na(data[, i]), i] <- ""
}
data2 <- as.list(data)
data2 <- lapply(data2, function(x) {x[x != ""]})
data2 <- list_to_matrix(data2)
m = make_comb_mat(data2, mode = "distinct")
ss = set_size(m)
cs = comb_size(m)
set_order <- order(ss)
comb_order <- order(comb_degree(m), -cs)

# View data
head(data)

# Create visualization
# Upset Plot
p <- as.ggplot(function(){
  top_annotation <- HeatmapAnnotation(
    Intersections = anno_barplot(
      cs, ylim = c(0, max(cs)*1.1), 
      border = FALSE, 
      gp = gpar(fill = "#000000", fontsize = 10), 
      height = unit(5, "cm")
    ), 
    annotation_name_side = "left", 
    annotation_name_rot = 90
  )
  
  left_annotation <- rowAnnotation(
    Numbers = anno_barplot(-ss, axis_param = list(
      at = seq(-max(ss), 0, round(max(ss)/5)),
      labels = rev(seq(0, max(ss), round(max(ss)/5))),
      labels_rot = 0),
      baseline = 0,
      border = FALSE, 
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
https://openbiox.github.io/Bizard/Hiplot/177-upset-plot.html
