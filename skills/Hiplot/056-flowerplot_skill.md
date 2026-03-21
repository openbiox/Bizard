# Skill: Flower plot (R)

## Category
Hiplot

## When to Use
Flower plot with multiple sets.

## Required R Packages
- data.table
- flowerplot
- ggplotify
- jsonlite

## Minimal Reproducible Code
```r
# Load packages
library(data.table)
library(flowerplot)
library(ggplotify)
library(jsonlite)

# Prepare data
# Load data
data <- data.table::fread(jsonlite::read_json("https://hiplot.cn/ui/basic/flowerplot/data.json")$exampleData$textarea[[1]])
data <- as.data.frame(data)

# View data
head(data)

# Create visualization
# Flower plot
p <- as.ggplot(function(){
  flowerplot(
    flower_dat = data,
    angle = 90,
    a = 0.5,
    b = 2,
    r = 1,
    ellipse_col = "RdBu",
    circle_col = "#FFFFFF",
    label_text_cex = 1
  )})

p
```

## Key Parameters
- `fill`: Maps a variable to fill color for group comparison
- `color`: Maps a variable to outline/point color

## Tips
- Adjust text size with `theme(text = element_text(size = 14))` for presentations
- See the full tutorial for additional customization options and advanced examples

## Full Tutorial
https://openbiox.github.io/Bizard/Hiplot/056-flowerplot.html
