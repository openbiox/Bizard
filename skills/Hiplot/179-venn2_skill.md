# Skill: Venn2 (R)

## Category
Hiplot

## When to Use
A Venn diagram is a diagramthat shows all possible logical relations between a finite collection of different sets. These diagrams depict elements as points in the plane, and sets as regions inside closed curves. A Venn diagram consists of multiple overlapping closed curves, usually circles, each representing a set. The points inside a curve labelled S represent elements of the set S, while points outside the boundary represent elements not in the set S. This lends to easily read visualizatio...

## Required R Packages
- data.table
- jsonlite
- venn

## Minimal Reproducible Code
```r
# Load packages
library(data.table)
library(jsonlite)
library(venn)

# Prepare data
# Load data
data <- data.table::fread(jsonlite::read_json("https://hiplot.cn/ui/basic/venn2/data.json")$exampleData[[1]]$textarea[[1]])
data <- as.data.frame(data)

# convert data structure
data_venn <- as.list(data)
data_venn <- lapply(data_venn, function(x) {
  x[is.na(x)] <- ""
  x <- x[x != ""]
  return(x)
})

# View data
head(data)

# Create visualization
# Venn2
col <- c("#E64B35FF","#4DBBD5FF","#00A087FF","#3C5488FF","#F39B7FFF")
venn(x=data_venn, opacity=0.8, ggplot=F, ilabels = TRUE, zcolor=col, box=F)
title(main = "Vene Plot (5 sets)", line = -1)
```

## Key Parameters
- `fill`: Maps a variable to fill color for group comparison
- `color`: Maps a variable to outline/point color

## Tips
- Adjust text size with `theme(text = element_text(size = 14))` for presentations
- See the full tutorial for additional customization options and advanced examples

## Full Tutorial
https://openbiox.github.io/Bizard/Hiplot/179-venn2.html
