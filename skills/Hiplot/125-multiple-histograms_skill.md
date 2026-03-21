# Skill: Multiple Histograms (R)

## Category
Hiplot

## When to Use
Multiple histograms are plotted on the same graph to compare differences between multiple sets of data.

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
data <- data.table::fread(jsonlite::read_json("https://hiplot.cn/ui/basic/multiple-histograms/data.json")$exampleData$textarea[[1]])
data <- as.data.frame(data)

# View data
head(data)

# Create visualization
# Multiple Histograms
p <- ggplot(data, aes(x = value, fill = type)) +
  geom_histogram(color = "black", alpha = 0.5, 
                 position = "identity", binwidth = 0.3) +
  scale_fill_manual(values = c("#BC3C29FF","#0072B5FF")) +
  theme_bw()

p
```

## Key Parameters
- `x`: Maps `value` to the x aesthetic
- `fill`: Maps `type` to the fill aesthetic
- `alpha`: Controls transparency (0 = fully transparent, 1 = opaque)
- `width`: Controls element width
- `position`: Position adjustment (identity, dodge, stack, fill)
- `theme`: Plot theme; tutorial uses `theme_bw()`

## Tips
- Use `theme_minimal()` or `theme_bw()` for clean, publication-ready plots
- Customize color scales with `scale_fill_manual()` or `scale_color_brewer()`
- See the full tutorial for additional customization options and advanced examples

## Full Tutorial
https://openbiox.github.io/Bizard/Hiplot/125-multiple-histograms.html
