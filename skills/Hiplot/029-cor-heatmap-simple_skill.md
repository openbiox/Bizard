# Skill: Simplified Correlation Heatmap (R)

## Category
Hiplot

## When to Use
Simplified variables correlation heatmap

## Required R Packages
- data.table
- ggplot2
- jsonlite
- sigminer

## Minimal Reproducible Code
```r
# Load packages
library(data.table)
library(ggplot2)
library(jsonlite)
library(sigminer)

# Prepare data
# Load data
data <- data.table::fread(jsonlite::read_json("https://hiplot.cn/ui/basic/cor-heatmap-simple/data.json")$exampleData$textarea[[1]])
data <- as.data.frame(data)

# View data
head(data)

# Create visualization
# Simplified Correlation Heatmap
p <- show_cor(
  data = data,
  x_vars = c("mpg","cyl","disp"),
  y_vars = c("wt","hp","drat"),
  cor_method = "pearson",
  vis_method = "square",
  lab = T,
  test = T,
  hc_order = F,
  legend.title = "Corr"
  ) +
  ggtitle("") +
  labs(x="", y="") +
  theme_bw() +
  theme(text = element_text(family = "Arial"),
        plot.title = element_text(size = 12,hjust = 0.5),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 10),
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
        legend.position = "right",
        legend.direction = "vertical",
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 10))

p
```

## Key Parameters
- `position`: Position adjustment (identity, dodge, stack, fill)
- `theme`: Plot theme; tutorial uses `theme_bw()`
- `fill`: Maps a variable to fill color for group comparison
- `color`: Maps a variable to outline/point color

## Tips
- Use `theme_minimal()` or `theme_bw()` for clean, publication-ready plots
- Adjust text size with `theme(text = element_text(size = 14))` for presentations
- See the full tutorial for additional customization options and advanced examples

## Full Tutorial
https://openbiox.github.io/Bizard/Hiplot/029-cor-heatmap-simple.html
