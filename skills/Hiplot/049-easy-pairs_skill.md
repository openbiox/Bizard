# Skill: Easy Pairs (R)

## Category
Hiplot

## When to Use
Display a matrix of plots for viewing correlation relationship and distributions of multiple variables.

## Required R Packages
- GGally
- data.table
- jsonlite

## Minimal Reproducible Code
```r
# Load packages
library(GGally)
library(data.table)
library(jsonlite)

# Prepare data
# Load data
data <- data.table::fread(jsonlite::read_json("https://hiplot.cn/ui/basic/easy-pairs/data.json")$exampleData[[1]]$textarea[[1]])
data <- as.data.frame(data)

# View data
head(data)

# Create visualization
# Easy Pairs
p <- ggpairs(data, columns = c("total_bill", "time", "tip"),
             mapping = aes_string(color = "gender")) +
  ggtitle("Easy Pairs") +
  scale_fill_manual(values = c("#3B4992FF","#EE0000FF")) +
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
- `position`: Position adjustment (identity, dodge, stack, fill)
- `theme`: Plot theme; tutorial uses `theme_bw()`
- `fill`: Maps a variable to fill color for group comparison
- `color`: Maps a variable to outline/point color

## Tips
- Use `theme_minimal()` or `theme_bw()` for clean, publication-ready plots
- Customize color scales with `scale_fill_manual()` or `scale_color_brewer()`
- See the full tutorial for additional customization options and advanced examples

## Full Tutorial
https://openbiox.github.io/Bizard/Hiplot/049-easy-pairs.html
