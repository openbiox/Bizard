# Skill: Rose Chart (R)

## Category
Hiplot

## When to Use
The rose chart is a column chart drawn in polar coordinates. The radius of the arc is used to indicate the size of the data.

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
data <- data.table::fread(jsonlite::read_json("https://hiplot.cn/ui/basic/rose-chart/data.json")$exampleData$textarea[[1]])
data <- as.data.frame(data)

# Convert data structure
data[, 2] <- factor(data[, 2], levels = unique(data[, 2]))

# View data
head(data)

# Create visualization
# Rose Chart
p <- ggplot(data, aes(x = Sample, y = Freq)) +
  geom_col(aes(fill = Group), width = 0.9, size = 0, alpha = 0.8) +
  coord_polar() +
  ggtitle("Rose Chart") +
  scale_fill_manual(values = c("#E64B35FF", "#4DBBD5FF")) +
  theme_bw() +
  theme(aspect.ratio = 1,
        axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black"),
        legend.title = element_blank(),
        legend.position = "bottom",
        plot.title = element_text(hjust = 0.5))

p
```

## Key Parameters
- `x`: Maps `Sample` to the x aesthetic
- `y`: Maps `Freq` to the y aesthetic
- `fill`: Maps `Group` to the fill aesthetic
- `alpha`: Controls transparency (0 = fully transparent, 1 = opaque)
- `width`: Controls element width
- `position`: Position adjustment (identity, dodge, stack, fill)
- `theme`: Plot theme; tutorial uses `theme_bw()`

## Tips
- Use `theme_minimal()` or `theme_bw()` for clean, publication-ready plots
- Customize color scales with `scale_fill_manual()` or `scale_color_brewer()`
- See the full tutorial for additional customization options and advanced examples

## Full Tutorial
https://openbiox.github.io/Bizard/Hiplot/157-rose-chart.html
