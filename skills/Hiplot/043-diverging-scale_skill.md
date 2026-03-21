# Skill: Diverging Scale (R)

## Category
Hiplot

## When to Use
The diverging scale is a graph that maps a continuous, quantitative input to a continuous fixed interpolator.

## Required R Packages
- data.table
- ggcharts
- jsonlite

## Minimal Reproducible Code
```r
# Load packages
library(data.table)
library(ggcharts)
library(jsonlite)

# Prepare data
# Load data
data <- data.table::fread(jsonlite::read_json("https://hiplot.cn/ui/basic/diverging-scale/data.json")$exampleData$textarea[[1]])
data <- as.data.frame(data)

# convert data structure
data <- dplyr::transmute(.data = data, x = model, y = scale(hp))

# View data
head(data)

# Create visualization
# Diverging Scale Barplot
fill_colors <- c("#C20B01", "#196ABD")
fill_colors <- fill_colors[c(any(data[, "y"] > 0), any(data[, "y"] < 0))]
p <- diverging_bar_chart(data = data, x = x, y = y, bar_colors = fill_colors,
                         text_color = '#000000') + 
  theme(axis.text.x = element_text(color = "#000000"),
        axis.title.x = element_text(colour = "#000000"),
        axis.title.y = element_text(colour = "#000000"),
        plot.background = element_blank()) + 
  labs(x = "model", y = "scale(hp)", title = "")

p
```

## Key Parameters
- `fill`: Maps a variable to fill color for group comparison
- `color`: Maps a variable to outline/point color

## Tips
- Adjust text size with `theme(text = element_text(size = 14))` for presentations
- See the full tutorial for additional customization options and advanced examples

## Full Tutorial
https://openbiox.github.io/Bizard/Hiplot/043-diverging-scale.html
