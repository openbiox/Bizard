# Skill: Waterfalls (R)

## Category
Hiplot

## When to Use
The waterfall chart is used to display the cumulative effect of sequentially introduced positive or negative values . These intermediate values can either be time based or category based.

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
data <- data.table::fread(jsonlite::read_json("https://hiplot.cn/ui/basic/waterfalls/data.json")$exampleData$textarea[[1]])
data <- as.data.frame(data)

# View data
head(data)

# Create visualization
# Waterfalls
p <- waterfall(data, rect_text_labels = data$value, rect_text_size = 1,
    rect_text_labels_anchor = "centre", calc_total = T,
    total_axis_text = "Total", total_rect_text = sum(data$value),
    total_rect_color = "steelblue", total_rect_text_color = "black",
    rect_width = 0.7, rect_border = "black", draw_lines = TRUE,
    linetype = 2, fill_by_sign = F, 
    fill_colours = c("#E64B35FF","#4DBBD5FF","#00A087FF","#3C5488FF","#F39B7FFF",
                     "#8491B4FF"),
    scale_y_to_waterfall = T) +
  theme_bw() +
  theme(axis.text = element_text(size = 12),
        plot.title = element_text(hjust = 0.5)) +
  labs(title = "Waterfalls Plot")

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
https://openbiox.github.io/Bizard/Hiplot/186-waterfalls.html
