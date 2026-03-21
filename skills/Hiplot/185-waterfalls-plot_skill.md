# Skill: Waterfalls Plot2 (R)

## Category
Hiplot

## When to Use
Used to visualize changes in data, with the difference from version 1 being the ability to customize the colors for upward and downward values.

## Required R Packages
- data.table
- ggplot2
- jsonlite
- waterfalls

## Minimal Reproducible Code
```r
# Load packages
library(data.table)
library(ggplot2)
library(jsonlite)
library(waterfalls)

# Prepare data
# Load data
data <- data.table::fread(jsonlite::read_json("https://hiplot.cn/ui/basic/waterfalls-plot/data.json")$exampleData$textarea[[1]])
data <- as.data.frame(data)

# convert data structure
data[["name"]] <- factor(data[["name"]], levels = data[["name"]])
data$fill <- ifelse(data$value > 0, "#B71C1C", "#1B5E20")

# View data
head(data)

# Create visualization
# Waterfalls Plot2
p <- waterfall(data, calc_total = T, rect_width = 0.7, fill_by_sign = F,
               fill_colours = data$fill, total_rect_color = "#1E065D") +
  theme_bw()

p
```

## Key Parameters
- `width`: Controls element width
- `theme`: Plot theme; tutorial uses `theme_bw()`
- `fill`: Maps a variable to fill color for group comparison
- `color`: Maps a variable to outline/point color

## Tips
- Use `theme_minimal()` or `theme_bw()` for clean, publication-ready plots
- Adjust text size with `theme(text = element_text(size = 14))` for presentations
- See the full tutorial for additional customization options and advanced examples

## Full Tutorial
https://openbiox.github.io/Bizard/Hiplot/185-waterfalls-plot.html
