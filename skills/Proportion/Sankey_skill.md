# Skill: Sankey Diagram (R)

## Category
Proportion

## When to use
A [Sankey diagram](https://www.data-to-viz.com/graph/sankey.html) allows to study flows. Entities (nodes) are represented by rectangles or text. Arrows or arcs are used to show flows between them. In `R`, the `networkD3` package is the best way to build them.

## Required R packages
- dplyr
- ggalluvial
- ggplot2
- networkD3
- openxlsx
- readxl
- tidyverse
- webshot

## Minimal reproducible code
```r
# Basic plotting
p1 <- sankeyNetwork(Links = drugs, Nodes = nodes,
              Source = "IDsource", Target = "IDtarget",
              Value = "value", NodeID = "name") 
              
p1
```

## Full tutorial
https://openbiox.github.io/Bizard/Proportion/Sankey.html
