# Skill: Arc Diagram (R)

## Category
Proportion

## When to use
The arc diagram is a diagram connected by arcs, showing the relationships between nodes.

## Required R packages
- colormap
- ggraph
- igraph
- patchwork
- tidyverse
- viridis

## Minimal reproducible code
```r
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
  )

p
```

## Full tutorial
https://openbiox.github.io/Bizard/Proportion/ArcDiagram.html
