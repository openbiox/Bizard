# Skill: Line (errorbar) (R)

## Category
Hiplot

## When to Use
The error line mainly indicates the error range of each data point and shows the potential error or uncertainty relative to each data in the series.

## Required R Packages
- data.table
- ggpubr
- jsonlite

## Minimal Reproducible Code
```r
# Load packages
library(data.table)
library(ggpubr)
library(jsonlite)

# Prepare data
# Load data
data <- data.table::fread(jsonlite::read_json("https://hiplot.cn/ui/basic/line-errorbar/data.json")$exampleData$textarea[[1]])
data <- as.data.frame(data)

# Convert data structure
data[, 3] <- factor(data[, 3], levels = unique(data[, 3]))

# View data
head(data)

# Create visualization
# Line (errorbar)
p <- ggline(
  data, x = "Group1", y = "Value", color = "Group2",
  add = "mean_se", title = "Line plot with errorbar", palette = "npg") +
  stat_compare_means(aes_(group = as.name("Group2"))) +
  theme_bw() +
  theme(text = element_text(family = "Arial"),
        plot.title = element_text(size = 12, hjust = 0.5),
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
- `stat`: Statistical transformation to use
- `theme`: Plot theme; tutorial uses `theme_bw()`

## Tips
- Use `theme_minimal()` or `theme_bw()` for clean, publication-ready plots
- Adjust text size with `theme(text = element_text(size = 14))` for presentations
- See the full tutorial for additional customization options and advanced examples

## Full Tutorial
https://openbiox.github.io/Bizard/Hiplot/093-line-errorbar.html
