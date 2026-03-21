# Skill: Dendrogram (R)

## Category
Composition

## When to Use
A dendrogram is a graphical representation of hierarchical relationships between objects. It is widely used in cluster analysis, especially hierarchical clustering, to visualize the similarity or distance between data points.

## Required R Packages
- collapsibleTree
- dendextend
- ggraph
- igraph
- tidyverse

## Minimal Reproducible Code
```r
ggraph(mygraph_unique, layout = 'dendrogram', circular = FALSE) + 
  geom_edge_diagonal() +
  geom_node_point() +
  theme_void()
```

## Full Tutorial
https://openbiox.github.io/Bizard/Composition/Dendrogram.html
