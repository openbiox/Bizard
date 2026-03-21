# Skill: Dendrogram (R)

## Category
Hiplot

## When to Use
The dendrogram is a diagram representing a tree. This diagrammatic representation is frequently used in different contexts:In hierarchical clustering, it illustrates the arrangement of the clusters produced by the corresponding analyses.

## Required R Packages
- ape
- data.table
- ggplotify
- jsonlite

## Minimal Reproducible Code
```r
# Load packages
library(ape)
library(data.table)
library(ggplotify)
library(jsonlite)

# Prepare data
# Load data
data <- data.table::fread(jsonlite::read_json("https://hiplot.cn/ui/basic/dendrogram/data.json")$exampleData$textarea[[1]])
data <- as.data.frame(data)

# convert data structure
data <- data[, -1]

# View data
head(data)

# Create visualization
# Dendrogram
d <- dist(t(data), method = "euclidean")
hc <- hclust(d, method = "complete")
clus <- cutree(hc, 4)

p <- as.ggplot(function() {
  par(mar = c(5, 5, 10, 5), mgp = c(2.5, 1, 0))
  plot(as.phylo(hc),
       type = "phylogram",
       tip.color = c("#00468bff","#ed0000ff","#42b540ff","#0099b4ff")[clus], 
       label.offset = 1,
       cex = 1, font = 2, use.edge.length = T
       )
  title("Dendrogram Plot", line = 1)
  })

p
```

## Key Parameters
- `fill`: Maps a variable to fill color for group comparison
- `color`: Maps a variable to outline/point color

## Tips
- Adjust text size with `theme(text = element_text(size = 14))` for presentations
- See the full tutorial for additional customization options and advanced examples

## Full Tutorial
https://openbiox.github.io/Bizard/Hiplot/037-dendrogram.html
