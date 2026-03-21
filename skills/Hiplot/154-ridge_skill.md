# Skill: Ridge (R)

## Category
Hiplot

## When to Use
The ridge map is a graph that connects points and forms a ridge.

## Required R Packages
- data.table
- ggplot2
- ggridges
- ggthemes
- jsonlite

## Minimal Reproducible Code
```r
# Load packages
library(data.table)
library(ggplot2)
library(ggridges)
library(ggthemes)
library(jsonlite)

# Prepare data
# Load data
data <- data.table::fread(jsonlite::read_json("https://hiplot.cn/ui/basic/ridge/data.json")$exampleData$textarea[[1]])
data <- as.data.frame(data)

# Convert data structure
data$group <- factor(data$group, levels = unique(data$group)[length(unique(data$group)):1])

# View data
head(data)

# Create visualization
# Ridge
p <- ggplot(data, aes(x = value, y = group, fill = group, col = group)) +
  geom_density_ridges(scale = 5, alpha = 0.8) +
  labs(x = "value", y = "group") +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "none") +
  ggtitle("Ridge Plot") +
  guides(color = guide_legend(reverse = TRUE),
         fill = guide_legend(reverse = TRUE)) +
  scale_fill_manual(values = c("#e04d39","#5bbad6","#1e9f86")) +
  scale_color_manual(values = c("#e04d39","#5bbad6","#1e9f86")) +
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
- `x`: Maps `value` to the x aesthetic
- `y`: Maps `group` to the y aesthetic
- `fill`: Maps `group` to the fill aesthetic
- `alpha`: Controls transparency (0 = fully transparent, 1 = opaque)
- `position`: Position adjustment (identity, dodge, stack, fill)
- `stat`: Statistical transformation to use
- `theme`: Plot theme; tutorial uses `theme_stata()`

## Tips
- Customize color scales with `scale_fill_manual()` or `scale_color_brewer()`
- Adjust text size with `theme(text = element_text(size = 14))` for presentations
- See the full tutorial for additional customization options and advanced examples

## Full Tutorial
https://openbiox.github.io/Bizard/Hiplot/154-ridge.html
