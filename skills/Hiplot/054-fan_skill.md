# Skill: Fan Plot (R)

## Category
Hiplot

## When to Use
The pie chart is a statistical chart designed to clearly show the percentage of each data group by the size of the pie.

## Required R Packages
- data.table
- ggplotify
- jsonlite
- plotrix

## Minimal Reproducible Code
```r
# Load packages
library(data.table)
library(ggplotify)
library(jsonlite)
library(plotrix)

# Prepare data
# Load data
data <- data.table::fread(jsonlite::read_json("https://hiplot.cn/ui/basic/fan/data.json")$exampleData$textarea[[1]])
data <- as.data.frame(data)

# View data
head(data)

# Create visualization
# Fan Plot
p <- as.ggplot(function() {
  fan.plot(data[, 2], main = "", labels = as.character(data[, 1]),
           col = c("#E64B35FF","#4DBBD5FF","#00A087FF","#3C5488FF"))
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
https://openbiox.github.io/Bizard/Hiplot/054-fan.html
