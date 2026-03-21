# Skill: Pyramid Chart (R)

## Category
Hiplot

## When to Use
The pyramid chart is a pyramid-like figure that distributes data on both sides of a central axis.

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
data <- data.table::fread(jsonlite::read_json("https://hiplot.cn/ui/basic/pyramid-chart/data.json")$exampleData$textarea[[1]])
data <- as.data.frame(data)

# View data
head(data)

# Create visualization
# Pyramid Chart
p <- pyramid_chart(data = data, x = age, y = pop, group = sex, 
                   title = "", sort = "no", bar_colors = c("#C20B01","#196ABD")) +
  theme(text = element_text(family = "Arial"),
        plot.title = element_text(size = 12,hjust = 0.5),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 10),
        axis.text.x = element_text(angle = 0, hjust = 0.5,vjust = 1),
        legend.position = "right",
        legend.direction = "vertical",
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 10))

p
```

## Key Parameters
- `position`: Position adjustment (identity, dodge, stack, fill)
- `fill`: Maps a variable to fill color for group comparison
- `color`: Maps a variable to outline/point color

## Tips
- Adjust text size with `theme(text = element_text(size = 14))` for presentations
- See the full tutorial for additional customization options and advanced examples

## Full Tutorial
https://openbiox.github.io/Bizard/Hiplot/144-pyramid-chart.html
