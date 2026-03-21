# Skill: Treemap (R)

## Category
Hiplot

## When to Use
Tree map is a kind of tree structure diagram that graphical form to represent hierarchy structure.

## Required R Packages
- data.table
- jsonlite
- treemap

## Minimal Reproducible Code
```r
# Load packages
library(data.table)
library(jsonlite)
library(treemap)

# Prepare data
# Load data
data <- data.table::fread(jsonlite::read_json("https://hiplot.cn/ui/basic/treemap/data.json")$exampleData$textarea[[1]])
data <- as.data.frame(data)

# View data
head(data)

# Create visualization
# Treemap
treemap(data, index = colnames(data)[1], vSize = colnames(data)[2],
        vColor = colnames(data)[1], type = "index", title = "", 
        algorithm = "pivotSize", sortID = colnames(data)[1], border.lwds = 1,
        fontcolor.labels = "#000000", inflate.labels = F, overlap.labels = 0.5,
        fontfamily.title = "Arial", fontfamily.legend = "Arial",
        fontfamily.labels = "Arial", 
        palette = c("#E64B35FF","#4DBBD5FF","#00A087FF","#3C5488FF","#F39B7FFF"), 
        aspRatio = 6 / 6)
```

## Key Parameters
- `fill`: Maps a variable to fill color for group comparison
- `color`: Maps a variable to outline/point color

## Tips
- Adjust text size with `theme(text = element_text(size = 14))` for presentations
- See the full tutorial for additional customization options and advanced examples

## Full Tutorial
https://openbiox.github.io/Bizard/Hiplot/173-treemap.html
