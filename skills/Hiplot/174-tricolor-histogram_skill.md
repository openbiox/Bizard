# Skill: Tricolor Histogram (R)

## Category
Hiplot

## When to Use
The tricolored histogram divides the histogram into three regions: low-value zone, middle-value zone, and high-value zone, using three different colors.

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
data <- data.table::fread(jsonlite::read_json("https://hiplot.cn/ui/basic/tricolor-histogram/data.json")$exampleData$textarea[[1]])
data <- as.data.frame(data)

# convert data structure
data$draw_color <- ifelse(data$value < 5, "#F44336",
                          ifelse(data$value > 7, "#006064", "#3F51B5")
)

# View data
head(data)

# Create visualization
# Tricolor Histogram
p <- ggplot(data, aes(x = values, fill = draw_color)) +
  geom_histogram(alpha = 0.5, binwidth = 0.05, position = "identity") +
    scale_fill_manual(values = c("#3F51B5", "#006064", "#F44336")) +
  theme_bw()

p
```

## Key Parameters
- `x`: Maps `values` to the x aesthetic
- `fill`: Maps `draw_color` to the fill aesthetic
- `alpha`: Controls transparency (0 = fully transparent, 1 = opaque)
- `width`: Controls element width
- `position`: Position adjustment (identity, dodge, stack, fill)
- `theme`: Plot theme; tutorial uses `theme_bw()`

## Tips
- Use `theme_minimal()` or `theme_bw()` for clean, publication-ready plots
- Customize color scales with `scale_fill_manual()` or `scale_color_brewer()`
- See the full tutorial for additional customization options and advanced examples

## Full Tutorial
https://openbiox.github.io/Bizard/Hiplot/174-tricolor-histogram.html
