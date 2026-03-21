# Skill: Histogram (R)

## Category
Hiplot

## When to Use
Histogram refers to the distribution of continuous variable data by a series of vertical stripes or line segments with different heights.

## Required R Packages
- data.table
- ggplot2
- ggthemes
- jsonlite

## Minimal Reproducible Code
```r
# Load packages
library(data.table)
library(ggplot2)
library(ggthemes)
library(jsonlite)

# Prepare data
# Load data
data <- data.table::fread(jsonlite::read_json("https://hiplot.cn/ui/basic/histogram/data.json")$exampleData$textarea[[1]])
data <- as.data.frame(data)

# Convert data structure
data[, 2] <- factor(data[, 2], levels = unique(data[, 2]))

# View data
head(data)

# Create visualization
# Histogram
p <- ggplot(data, aes(x=Value, fill=Group2)) +
  geom_histogram(alpha = 1, bins = 12, col = "white") +
  ggtitle("Histogram Plot") +
  scale_fill_manual(values = c("#e04d39","#5bbad6","#1e9f86")) +
  theme_stata() +
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
- `x`: Maps `Value` to the x aesthetic
- `fill`: Maps `Group2` to the fill aesthetic
- `alpha`: Controls transparency (0 = fully transparent, 1 = opaque)
- `position`: Position adjustment (identity, dodge, stack, fill)
- `stat`: Statistical transformation to use
- `theme`: Plot theme; tutorial uses `theme_stata()`

## Tips
- Customize color scales with `scale_fill_manual()` or `scale_color_brewer()`
- Adjust text size with `theme(text = element_text(size = 14))` for presentations
- See the full tutorial for additional customization options and advanced examples

## Full Tutorial
https://openbiox.github.io/Bizard/Hiplot/088-histogram.html
