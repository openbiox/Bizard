# Skill: Matrix Bubble (R)

## Category
Hiplot

## When to Use
The color matrix bubble is used to visualize the expression matrix data of multiple genes (rows) in various cells (columns).

## Required R Packages
- data.table
- ggalluvial
- ggplot2
- jsonlite

## Minimal Reproducible Code
```r
# Load packages
library(data.table)
library(ggalluvial)
library(ggplot2)
library(jsonlite)

# Prepare data
# Load data
data <- data.table::fread(jsonlite::read_json("https://hiplot.cn/ui/basic/matrix-bubble/data.json")$exampleData$textarea[[1]])
data <- as.data.frame(data)

# Convert data structure
data[, 1] <- factor(data[, 1], levels = unique(data[, 1]))
data[, 2] <- factor(data[, 2], levels = unique(data[, 2]))

# View data
head(data)

# Create visualization
# Matrix Bubble
p <- ggplot(data = data, aes(x = x, y = y, size = value, color = y)) +
  geom_point(alpha = 1) +
  labs(title = "Matrix Bubble") +
  guides(color = FALSE) +
  theme(panel.background = element_blank(),
        panel.grid.major = element_line(colour = "gray"),
        strip.background = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA),
        panel.spacing = unit(0, "lines"),
        plot.title = element_text(size = 12, hjust = 0.5),
        text = element_text(family = "Arial"),
        legend.title = element_text(size = 10),
        axis.text.x = element_text(angle=0, hjust=0.5, vjust=1)) +
  facet_grid(~group, scales = 'fixed', margins = F) +
  scale_color_manual(values = c(
    "#3B4992FF","#EE0000FF","#008B45FF","#631879FF","#008280FF","#BB0021FF",
    "#5F559BFF","#A20056FF","#808180FF","#1B1919FF"))

p
```

## Key Parameters
- `x`: Maps `x` to the x aesthetic
- `y`: Maps `y` to the y aesthetic
- `size`: Maps `value` to the size aesthetic
- `color`: Maps `y` to the color aesthetic
- `alpha`: Controls transparency (0 = fully transparent, 1 = opaque)

## Tips
- Customize color scales with `scale_fill_manual()` or `scale_color_brewer()`
- Use `facet_wrap()` or `facet_grid()` to create multi-panel plots by group
- See the full tutorial for additional customization options and advanced examples

## Full Tutorial
https://openbiox.github.io/Bizard/Hiplot/118-matrix-bubble.html
