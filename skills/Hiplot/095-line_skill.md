# Skill: Line (R)

## Category
Hiplot

## When to Use
The line chart is a statistical chart that USES a linear or logarithmic scale to draw data in a two - or three-dimensional view to show the data set or track the characteristics of the data over time.

## Required R Packages
- data.table
- ggplot2
- ggthemes
- jsonlite

## Minimal Reproducible Code
```r
# Load packages
library(data.table)
library(ggplot2)
library(ggthemes)
library(jsonlite)

# Prepare data
# Load data
data <- data.table::fread(jsonlite::read_json("https://hiplot.cn/ui/basic/line/data.json")$exampleData$textarea[[1]])
data <- as.data.frame(data)

# Convert data structure
data[,3] <- factor(data[,3], levels = unique(data[,3]))

# View data
head(data)

# Create visualization
# Line
p <- ggplot(data, aes(x = Value1, y = Value2)) +
  geom_line(alpha = 1, aes(color = Group, linetype = Group)) +
  geom_point(aes(color = Group, shape = Group)) +
  ggtitle("Line Regression Plot") +
  scale_fill_manual(values = c("#e04d39","#5bbad6")) +
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
- `x`: Maps `Value1` to the x aesthetic
- `y`: Maps `Value2` to the y aesthetic
- `color`: Maps `Group` to the color aesthetic
- `shape`: Maps `Group` to the shape aesthetic
- `alpha`: Controls transparency (0 = fully transparent, 1 = opaque)
- `position`: Position adjustment (identity, dodge, stack, fill)
- `stat`: Statistical transformation to use
- `theme`: Plot theme; tutorial uses `theme_stata()`

## Tips
- Customize color scales with `scale_fill_manual()` or `scale_color_brewer()`
- Adjust text size with `theme(text = element_text(size = 14))` for presentations
- See the full tutorial for additional customization options and advanced examples

## Full Tutorial
https://openbiox.github.io/Bizard/Hiplot/095-line.html
