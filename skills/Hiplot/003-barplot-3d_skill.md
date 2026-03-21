# Skill: 3D Barplot (R)

## Category
Hiplot

## When to Use
3D bar charts are used to provide a 3D look and feel for the data. The third dimension is often used for aesthetic reasons, but it does not improve data reading. Still intended to show comparisons between discrete categories.

## Required R Packages
- data.table
- ggplotify
- jsonlite
- plot3D

## Minimal Reproducible Code
```r
# Load packages
library(data.table)
library(ggplotify)
library(jsonlite)
library(plot3D)

# Prepare data
# Load data
data <- data.table::fread(jsonlite::read_json("https://hiplot.cn/ui/basic/barplot-3d/data.json")$exampleData$textarea[[1]])
data <- as.data.frame(data)

# convert data vector to a matrix
mat <- matrix(rep(1, nrow(data)), nrow = length(unique(data[, 2])))
rownames(mat) <- unique(data[, 2])
colnames(mat) <- unique(data[, 3])
for (i in 1:nrow(mat)) {
  for (j in seq_len(ncol(mat))) {
    mat[i, j] <- data[, 1][data[, 2] == rownames(mat)[i] &
      data[, 3] == colnames(mat)[j]]
  }
}

# View data
mat

# Create visualization
# 3D Barplot
p <- as.ggplot(function() {
  hist3D(
    x = 1:nrow(mat), y = seq_len(ncol(mat)), z = mat,
    bty = "g", phi = 20,
    theta = -55,
    xlab = colnames(data)[2],
    ylab = colnames(data)[3], zlab = colnames(data)[1],
    main = "3D Bar Plot", colkey = F,
    border = "black", shade = 0.8, axes = T,
    ticktype = "detailed", space = 0.3, d = 2, cex.axis = 0.3,
    colvar = as.numeric(as.factor(data[, 2])), alpha = 1,
    col = c("#E64B35FF","#4DBBD5FF","#00A087FF","#3C5488FF")
  )

  # Use text3D to label x axis
  text3D(
    x = 1:nrow(mat), y = rep(0.5, nrow(mat)), z = rep(3, nrow(mat)),
    labels = rownames(mat),
    add = TRUE, adj = 0, cex = 0.8
  )
  # Use text3D to label y axis
  text3D(
    x = rep(1, ncol(mat)), y = seq_len(ncol(mat)), z = rep(0, ncol(mat)),
# ... (see full tutorial for more)
```

## Key Parameters
- `alpha`: Controls transparency (0 = fully transparent, 1 = opaque)
- `fill`: Maps a variable to fill color for group comparison
- `color`: Maps a variable to outline/point color

## Tips
- Adjust text size with `theme(text = element_text(size = 14))` for presentations
- See the full tutorial for additional customization options and advanced examples

## Full Tutorial
https://openbiox.github.io/Bizard/Hiplot/003-barplot-3d.html
