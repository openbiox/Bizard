# Skill: Custom Icon Scatter (R)

## Category
Hiplot

## When to Use
A scatter plot with customizable icons.

## Required R Packages
- data.table
- echarts4r
- echarts4r.assets
- jsonlite

## Minimal Reproducible Code
```r
# Load packages
library(data.table)
library(echarts4r)
library(echarts4r.assets)
library(jsonlite)

# Prepare data
# Load data
data <- data.table::fread(jsonlite::read_json("https://hiplot.cn/ui/basic/custom-icon-scatter/data.json")$exampleData$textarea[[1]])
data <- as.data.frame(data)

# convert data structure
draw_data <- data.frame(
  x = data[["mpg"]],
  y = data[["wt"]],
  size = data[["qsec"]]
  )
rm(data)

# View data
head(draw_data)

# Create visualization
# Custom Icon Scatter
p <- draw_data |>
  e_charts(x) |>
  e_scatter(
    y,
    size,
    symbol = ea_icons("warning"),
    name = "warning"
    )

p
```

## Key Parameters
- `fill`: Maps a variable to fill color for group comparison
- `color`: Maps a variable to outline/point color

## Tips
- Adjust text size with `theme(text = element_text(size = 14))` for presentations
- See the full tutorial for additional customization options and advanced examples

## Full Tutorial
https://openbiox.github.io/Bizard/Hiplot/035-custom-icon-scatter.html
