# Skill: Gradient Scatter (R)

## Category
Hiplot

## When to Use
Two-dimensional spatial scatter to demonstrate multi-numerical variable relationships.

## Required R Packages
- data.table
- ggplot2
- grafify
- jsonlite

## Minimal Reproducible Code
```r
# Load packages
library(data.table)
library(ggplot2)
library(grafify)
library(jsonlite)

# Prepare data
# Load data
data <- data.table::fread(jsonlite::read_json("https://hiplot.cn/ui/basic/scatter-gradient/data.json")$exampleData[[1]]$textarea[[1]])
data <- as.data.frame(data)

# View data
head(data)

# Create visualization
# Gradient Scatter
p <- ggplot(data, aes(x = mpg, y = disp)) + 
  geom_point(aes(fill = gear), size = 5, alpha = 1, shape = 21, stroke = 0.5) +
  labs(fill = "gear", color = "gear") +
  theme_classic(base_size = 10) +
  theme(strip.background = element_blank()) +
  guides(x = guide_axis(angle = 0)) +
  scale_fill_gradient(low = "#00438E", high = "#E43535") +
  scale_color_gradient(low = "#00438E", high = "#E43535") + 
  guides(fill = guide_legend(title = "gear"),
         size = guide_legend(title = "gear")) +
  ggtitle("Scatter-gradient Plot") +
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
- `x`: Maps `mpg` to the x aesthetic
- `y`: Maps `disp` to the y aesthetic
- `fill`: Maps `gear` to the fill aesthetic
- `alpha`: Controls transparency (0 = fully transparent, 1 = opaque)
- `position`: Position adjustment (identity, dodge, stack, fill)

## Tips
- Customize color scales with `scale_fill_manual()` or `scale_color_brewer()`
- Adjust text size with `theme(text = element_text(size = 14))` for presentations
- See the full tutorial for additional customization options and advanced examples

## Full Tutorial
https://openbiox.github.io/Bizard/Hiplot/160-scatter-gradient.html
