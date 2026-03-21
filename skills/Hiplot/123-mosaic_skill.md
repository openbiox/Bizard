# Skill: Mosaic Ratio Plot (R)

## Category
Hiplot

## When to Use
Use mosaic blocks to show data proportions.

## Required R Packages
- DescTools
- data.table
- ggplotify
- jsonlite
- vcd

## Minimal Reproducible Code
```r
# Load packages
library(DescTools)
library(data.table)
library(ggplotify)
library(jsonlite)
library(vcd)

# Prepare data
# Load data
data <- data.table::fread(jsonlite::read_json("https://hiplot.cn/ui/basic/mosaic/data.json")$exampleData$textarea[[1]])
data <- as.data.frame(data)

# Convert data structure
tbl <- xtabs(~ Survived + PassengerClass + Gender, data)

# View data
head(data)

# Create visualization
# Mosaic Ratio Plot
p <- as.ggplot(function() {
  mosaic(tbl, shade = TRUE, legend = TRUE, main = "Mosaic Ratio Plot",
         gp = shading_binary(tbl, col = c("#3B4992FF","#EE0000FF")))
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
https://openbiox.github.io/Bizard/Hiplot/123-mosaic.html
