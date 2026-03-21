# Skill: Waffle Plot (R)

## Category
Hiplot

## When to Use
Create a Waffle Plot using R with the Hiplot platform's approach. Suitable for biomedical data visualization with publication-quality output.

## Required R Packages
- data.table
- jsonlite
- waffle

## Minimal Reproducible Code
```r
# Load packages
library(data.table)
library(jsonlite)
library(waffle)

# Prepare data
# Load data
data <- data.table::fread(jsonlite::read_json("https://hiplot.cn/ui/basic/waffle/data.json")$exampleData$textarea[[1]])
data <- as.data.frame(data)

# convert data structure
parts <- data[,2]

# View data
head(data)

# Create visualization
# Waffle Plot
p <- waffle(parts, rows = 8, size = 1, legend_pos = "right") +
  ggtitle("Waffle Plot") +
  scale_fill_manual(values = c("#e04d39","#5bbad6","#1e9f86")) +
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
- Customize color scales with `scale_fill_manual()` or `scale_color_brewer()`
- Adjust text size with `theme(text = element_text(size = 14))` for presentations
- See the full tutorial for additional customization options and advanced examples

## Full Tutorial
https://openbiox.github.io/Bizard/Hiplot/184-waffle.html
