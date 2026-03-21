# Skill: Sankey Diagram (R)

## Category
Proportion

## When to Use
A [Sankey diagram](https://www.data-to-viz.com/graph/sankey.html) allows to study flows. Entities (nodes) are represented by rectangles or text. Arrows or arcs are used to show flows between them. In `R`, the `networkD3` package is the best way to build them.

## Required R Packages
- dplyr
- ggalluvial
- ggplot2
- networkD3
- openxlsx
- readxl
- tidyverse
- webshot

## Minimal Reproducible Code
```r
# Load packages
library(dplyr)
library(ggalluvial)
library(ggplot2)
library(networkD3)
library(openxlsx)
library(readxl)

# Prepare data
#Read drug clinical dataset
drugs <- read.csv("https://bizard-1301043367.cos.ap-guangzhou.myqcloud.com/drugs.csv", stringsAsFactors = FALSE)
# Create a node data frame
nodes <- data.frame(
  name=c(as.character(drugs$source), 
         as.character(drugs$target)) %>% unique())
# Reformat
drugs$IDsource <- match(drugs$source, nodes$name)-1 
drugs$IDtarget <- match(drugs$target, nodes$name)-1


#Read drug clinical dataset
drug <- read.csv("https://bizard-1301043367.cos.ap-guangzhou.myqcloud.com/drug.csv", stringsAsFactors = FALSE)
levels(drug$`glucose（mmol/L）`) <- rev(levels(drug$`glucose（mmol/L）`))

# Create visualization
# Basic plotting
p1 <- sankeyNetwork(Links = drugs, Nodes = nodes,
              Source = "IDsource", Target = "IDtarget",
              Value = "value", NodeID = "name") 
              
p1
```

## Key Parameters
- `x`: Maps `time` to the x aesthetic
- `y`: Maps `value` to the y aesthetic
- `fill`: Maps `level` to the fill aesthetic
- `alpha`: Controls transparency (0 = fully transparent, 1 = opaque)
- `position`: Position adjustment (identity, dodge, stack, fill)
- `stat`: Statistical transformation to use

## Tips
- Adjust text size with `theme(text = element_text(size = 14))` for presentations
- See the full tutorial for additional customization options and advanced examples

## Full Tutorial
https://openbiox.github.io/Bizard/Proportion/Sankey.html
