# Skill: Chord Diagram (R)

## Category
Proportion

## When to use
Chord diagrams can use connecting lines or bars to represent the relationships between different objects. The connections in a chord diagram directly show the relationships between different objects; the width of the connection is proportional to the strength of the relationship, and the color of the connection can represent another mapping of the relationship, such as the type of relationship. The size of the sectors in the diagram represents the measurement of the objects.

## Required R packages
- chorddiag
- circlize
- dplyr
- ggraph
- htmlwidgets
- igraph
- readr
- readxl
- tidygraph
- tidyverse
- viridis

## Minimal reproducible code
```r
# Initialize the circular plot
circos.clear()
circos.initialize(factors = plot_data$factor, x = plot_data$x)

# Plot a circular scatter plot
circos.trackPlotRegion(factors = plot_data$factor, y = plot_data$y, track.height = 0.5, panel.fun = function(x, y) {
  circos.axis()
})
circos.trackPoints(plot_data$factor, plot_data$x, plot_data$y, col = "blue", pch = 16, cex = 0.5)

# Add gene labeling
circos.text(x = 9, y = 20, labels = "BRCA1 & BRCA2", 
            sector.index = "a", facing = "outside", niceFacing = TRUE, 
            adj = c(0, 0.5), cex = 0.7, col = "blue")

circos.text(x = 11, y = 20, labels = "ERBB2 & PIK3CA", 
            sector.index = "b", facing = "outside", niceFacing = TRUE, 
            adj = c(0, 0.5), cex = 0.7, col = "blue")

circos.text(x = 17, y = 20, labels = "PTEN & AKT1", 
            sector.index = "c", facing = "outside", niceFacing = TRUE, 
            adj = c(0, 0.5), cex = 0.7, col = "blue")

# clear circular plot
circos.clear()
```

## Full tutorial
https://openbiox.github.io/Bizard/Proportion/ChordDiagram.html
