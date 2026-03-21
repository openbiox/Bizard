# Skill: 3D-Scatter (R)

## Category
Hiplot

## When to Use
3D scatter plot is to apply a number of quantitative variables to different coaxes in space and combine different variables into coordinates in space, so as to clearly explain the interaction between the three quantitative variables.

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
data <- data.table::fread(jsonlite::read_json("https://hiplot.cn/ui/basic/scatter-3d/data.json")$exampleData[[1]]$textarea[[1]])
data <- as.data.frame(data)

# Convert data structure
col_idx <- which(colnames(data) == "group")
data[, col_idx] <- as.factor(data[, col_idx])
shapes <- 19
shape_idx <- ""

# View data
head(data)

# Create visualization
# 3D-Scatter
p <- as.ggplot(function() {
  plot3d <- scatter3D(data[, 1], data[, 2], data[, 3],
    pch = shapes, cex = 1,
    phi = 0, theta = 45, ticktype = "detailed",
    bty = "b2", colkey = FALSE, alpha = 1,
    xlab = colnames(data)[1], ylab = colnames(data)[2],
    zlab = colnames(data)[3],
    main = "3D-Scatter Plot",
    colvar = as.numeric(as.factor(data[, 4])),
    col = c("#e04d39","#5bbad6","#1e9f86")
  )
  
  legend("right", pch=19, legend = levels(data[, col_idx]),
         cex = 1.1, bty = 'n', xjust = 0.5, horiz = F,
         title = colnames(data)[col_idx],
         col = c("#e04d39","#5bbad6","#1e9f86"))
})

p
```

## Key Parameters
- `alpha`: Controls transparency (0 = fully transparent, 1 = opaque)
- `fill`: Maps a variable to fill color for group comparison
- `color`: Maps a variable to outline/point color

## Tips
- Adjust text size with `theme(text = element_text(size = 14))` for presentations
- See the full tutorial for additional customization options and advanced examples

## Full Tutorial
https://openbiox.github.io/Bizard/Hiplot/159-scatter-3d.html
