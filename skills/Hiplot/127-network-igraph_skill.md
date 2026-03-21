# Skill: Network (igraph) (R)

## Category
Hiplot

## When to Use
Network (igraph) can be used to visulize basic network based on igraph.

## Required R Packages
- RColorBrewer
- data.table
- ggplotify
- igraph
- jsonlite
- stringr

## Minimal Reproducible Code
```r
# Load packages
library(RColorBrewer)
library(data.table)
library(ggplotify)
library(igraph)
library(jsonlite)
library(stringr)

# Prepare data
# Load data
nodes_data <- data.table::fread(jsonlite::read_json("https://hiplot.cn/ui/basic/network-igraph/data.json")$exampleData[[1]]$textarea[[1]])
nodes_data <- as.data.frame(nodes_data)
edges_data <- data.table::fread(jsonlite::read_json("https://hiplot.cn/ui/basic/network-igraph/data.json")$exampleData[[1]]$textarea[[2]])
edges_data <- as.data.frame(edges_data)

# Convert data structure
nodes_data[,"type.label"] <- factor(nodes_data[,"type.label"], 
                                    levels = unique(nodes_data[,"type.label"]))
nodes_data$hiplot_color_type <- as.numeric(nodes_data[,"type.label"])
net <- graph_from_data_frame(d = edges_data, vertices = nodes_data, directed = T)
## Generate colors based on type
colrs <- c("#7f7f7f","#ff6347","#ffd700")
colrs2 <- c("#BC3C29FF","#0072B5FF","#E18727FF","#20854EFF","#7876B1FF",
            "#6F99ADFF","#FFDC91FF","#EE4C97FF")
V(net)$color <- colrs[V(net)$hiplot_color_type]
## Compute node degrees (#links) and use that to set node size
deg <- degree(net, mode="all")
V(net)$size <- deg*3
## Set label
V(net)$label.color <- "black"
V(net)$label <- NA
## Set edge width based on weight
weight_column <- edges_data$weight
E(net)$width <- weight_column/6
## Change arrow size and edge color
E(net)$arrow.size <- .2
E(net)$edge.color <- "gray80"
edge.start <- ends(net, es=E(net), names=F)[,1]
edge.col <- V(net)$color[edge.start]

# View data
head(nodes_data)
head(edges_data)

# Create visualization
# Network (igraph)
raw <- par()
p <- as.ggplot(function () {
  par(mar=c(8,2,2,2))
  radian.rescale <- function(x, start=0, direction=1) {
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
https://openbiox.github.io/Bizard/Hiplot/127-network-igraph.html
