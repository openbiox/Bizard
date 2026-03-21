# Skill: Scatter (R)

## Category
Hiplot

## When to Use
Two groups of data are used to form multiple coordinate points. By observing the distribution of coordinate points, it can judge whether there is correlation between variables or summarize the data processing mode of coordinate point distribution.

## Required R Packages
- data.table
- ggplot2
- jsonlite

## Minimal Reproducible Code
```r
# Load packages
library(data.table)
library(ggplot2)
library(jsonlite)

# Prepare data
# Load data
data <- data.table::fread(jsonlite::read_json("https://hiplot.cn/ui/basic/scatter/data.json")$exampleData$textarea[[1]])
data <- as.data.frame(data)

# View data
head(data)

# Create visualization
# Scatter
p <- ggplot(data, aes(x = Value1, y = Value2)) +
  geom_point(size = 1, alpha = 1, aes(color = Group, shape = Group)) +
  ggtitle("Scatter Plot") +
  scale_color_manual(values = c("#00468BFF", "#ED0000FF")) +
  theme_bw() +
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
- `x`: Maps `Value1` to the x aesthetic
- `y`: Maps `Value2` to the y aesthetic
- `color`: Maps `Group` to the color aesthetic
- `shape`: Maps `Group` to the shape aesthetic
- `alpha`: Controls transparency (0 = fully transparent, 1 = opaque)
- `position`: Position adjustment (identity, dodge, stack, fill)
- `theme`: Plot theme; tutorial uses `theme_bw()`

## Tips
- Use `theme_minimal()` or `theme_bw()` for clean, publication-ready plots
- Customize color scales with `scale_fill_manual()` or `scale_color_brewer()`
- See the full tutorial for additional customization options and advanced examples

## Full Tutorial
https://openbiox.github.io/Bizard/Hiplot/161-scatter.html
