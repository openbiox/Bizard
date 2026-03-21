# Skill: Arc Diagram (R)

## Category
Proportion

## When to Use
The arc diagram is a diagram connected by arcs, showing the relationships between nodes.

## Required R Packages
- colormap
- ggraph
- igraph
- patchwork
- tidyverse
- viridis

## Minimal Reproducible Code
```r
# Load packages
library(colormap)
library(ggraph)
library(igraph)
library(patchwork)
library(tidyverse)
library(viridis)

# Prepare data
# 1.Custom data
# `links` stores edge information, and `nodes` stores node information and node grouping information.
links <- data.frame(
source = c("A", "A", "A", "A", "B", "G", "G", "G", "G"),
  target = c("B", "C", "D", "F", "E", "H", "I", "J", "F")
)
nodes <- data.frame(
  point = c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J"),
  groups = c(
    "group-one", "group-one", "group-one", "group-one", "group-one",
    "group-one", "group-two", "group-two", "group-two", "group-two"
  )
)

head(links)
head(nodes)

# 2.Researchers co-authored network
# Copy the link information to a txt file, read it, and draw the plot.
data_dif <- read.csv("https://bizard-1301043367.cos.ap-guangzhou.myqcloud.com/Arc.txt", header = T, sep = " ")

head(data_dif[,1:5])

# 3.PPI network node and edge data
# Read PPI network information downloaded from GitHub
data_ppi <- read.csv("https://bizard-1301043367.cos.ap-guangzhou.myqcloud.com/string_interactions_short.tsv_1%20default%20edge.csv", header = TRUE)

head(data_ppi[,c(9,3)])

# Create visualization
# Basic arc diagram
mygraph <- graph_from_data_frame(links, vertices = nodes) # Generate graph structure

p <- ggraph(mygraph, layout = "linear") +
  geom_edge_arc(edge_colour = "black", edge_alpha = 0.3, edge_width = 0.4) +
  geom_node_point(color = "grey", size = 5) +
  geom_node_text(aes(label = name), repel = FALSE, size = 6, nudge_y = -0.15) +
  theme_void() +
  theme(
    legend.position = "none",
    plot.margin = unit(rep(2, 4), "cm")
# ... (see full tutorial for more)
```

## Key Parameters
- `color`: Maps `as` to the color aesthetic
- `size`: Maps `n` to the size aesthetic
- `alpha`: Controls transparency (0 = fully transparent, 1 = opaque)
- `width`: Controls element width
- `position`: Position adjustment (identity, dodge, stack, fill)
- `stat`: Statistical transformation to use
- `theme`: Plot theme; tutorial uses `theme_void()`

## Tips
- Customize color scales with `scale_fill_manual()` or `scale_color_brewer()`
- Adjust text size with `theme(text = element_text(size = 14))` for presentations
- See the full tutorial for additional customization options and advanced examples

## Full Tutorial
https://openbiox.github.io/Bizard/Proportion/ArcDiagram.html
