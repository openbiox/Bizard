# Skill: Chord Plot (R)

## Category
Hiplot

## When to Use
The complex interaction is visualized in the form of chord graph.

## Required R Packages
- circlize
- data.table
- ggplotify
- jsonlite

## Minimal Reproducible Code
```r
# Load packages
library(circlize)
library(data.table)
library(ggplotify)
library(jsonlite)

# Prepare data
# Load data
data <- data.table::fread(jsonlite::read_json("https://hiplot.cn/ui/basic/chord/data.json")$exampleData$textarea[[1]])
data <- as.data.frame(data)

# convert data structure
row.names(data) <- data[, 1]
data <- data[, -1]
data <- as.matrix(data)

# View data
head(data)

# Create visualization
# Chord Plot
Palette <- c("#E64B35FF","#4DBBD5FF","#00A087FF","#3C5488FF","#F39B7FFF",
             "#8491B4FF","#91D1C2FF","#DC0000FF","#7E6148FF","#B09C85FF")
grid.col <- c(Palette, Palette, Palette[1:5])
p <- as.ggplot(function() {
  chordDiagram(
    data, grid.col = grid.col, grid.border = NULL, transparency = 0.5,
    row.col = NULL, column.col = NULL,  order = NULL,
    directional = 0, # 1, -1, 0, 2
    direction.type = "diffHeight", # diffHeight and arrows
    diffHeight = convert_height(2, "mm"), reduce = 1e-5, xmax = NULL, 
    self.link = 2, symmetric = FALSE, keep.diagonal = FALSE, 
    preAllocateTracks = NULL,
    annotationTrack = c("name", "grid", "axis"),
    annotationTrackHeight = convert_height(c(3, 3), "mm"),
    link.border = NA, link.lwd = par("lwd"), link.lty = par("lty"), 
    link.sort = FALSE, link.decreasing = TRUE, link.largest.ontop = FALSE,
    link.visible = T, link.rank = NULL, link.overlap = FALSE,
    scale = F, group = NULL, big.gap = 10, small.gap = 1
    )
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
https://openbiox.github.io/Bizard/Hiplot/020-chord.html
