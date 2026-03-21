# Skill: Scatter2 (R)

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
data <- data.table::fread(jsonlite::read_json("https://hiplot.cn/ui/basic/scatter2/data.json")$exampleData[[1]]$textarea[[1]])
data <- as.data.frame(data)

# View data
head(data[,1:5])

# Create visualization
# scatter2
symsize <- data[,"gear"]
data[,"gear"] <- factor(data[,"gear"], levels = unique(data[,"gear"]))
p <- ggplot(data, aes(x = mpg, y = disp)) + 
  geom_point(alpha = 1, aes(size = gear, fill = gear), shape = 21, stroke = 0.5) +
  labs(fill = "gear", color = "gear") +
  guides(x = guide_axis(angle = 0),
         fill = guide_legend(title = "gear"),
         color = FALSE,
         size = guide_legend(title = "gear")) +
  ggtitle("Scatter2 Plot") +
  scale_fill_grafify() +
  theme_classic(base_size = 20) +
  theme(text = element_text(family = "Arial"),
        strip.background = element_blank(),
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
- `size`: Maps `gear` to the size aesthetic
- `fill`: Maps `gear` to the fill aesthetic
- `alpha`: Controls transparency (0 = fully transparent, 1 = opaque)
- `position`: Position adjustment (identity, dodge, stack, fill)

## Tips
- Customize color scales with `scale_fill_manual()` or `scale_color_brewer()`
- Adjust text size with `theme(text = element_text(size = 14))` for presentations
- See the full tutorial for additional customization options and advanced examples

## Full Tutorial
https://openbiox.github.io/Bizard/Hiplot/162-scatter2.html
