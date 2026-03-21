# Skill: Easy SOM (R)

## Category
Hiplot

## When to Use
Establish the SOM model and conduct the visulization.

## Required R Packages
- data.table
- jsonlite
- kohonen

## Minimal Reproducible Code
```r
# Load packages
library(data.table)
library(jsonlite)
library(kohonen)

# Prepare data
# Load data
data <- data.table::fread(jsonlite::read_json("https://hiplot.cn/ui/basic/easy-som/data.json")$exampleData$textarea[[1]])
data <- as.data.frame(data)

# convert data structure
target <- data[,1]
target <- factor(target, levels = unique(target))
data <- data[,-1]
data <- as.data.frame(data)
for (i in 1:ncol(data)) {
  data[,i] <- as.numeric(data[,i])
}
data <- as.matrix(data)
set.seed(7)
kohmap <- xyf(scale(data), target, grid = somgrid(xdim=6, ydim=4, topo="hexagonal"), rlen=100)

color_key <- c("#A50026","#D73027","#F46D43","#FDAE61","#FEE090","#FFFFBF","#E0F3F8",
               "#ABD9E9","#74ADD1","#4575B4","#313695")
colors <- function (n, alpha, rev = FALSE) {
  colorRampPalette(color_key)(n)
}

# View data
head(data[,1:5])

# Create visualization
# Easy SOM
p <- function () {
  par(mfrow = c(3,2))
  xyfpredictions <- classmat2classvec(getCodes(kohmap, 2))
  plot(kohmap, type="counts", col = as.integer(target),
       palette.name = colors,
       pchs = as.integer(target), 
       main = "Counts plot", shape = "straight", border = NA)
  
  som.hc <- cutree(hclust(object.distances(kohmap, "codes")), 3)
  add.cluster.boundaries(kohmap, som.hc)

  plot(kohmap, type="mapping",
       labels = as.integer(target), col = colors(3)[as.integer(target)],
       palette.name = colors,
       shape = "straight",
       main = "Mapping plot")

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
https://openbiox.github.io/Bizard/Hiplot/050-easy-som.html
