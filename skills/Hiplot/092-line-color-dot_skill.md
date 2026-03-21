# Skill: Line (Color Dot) (R)

## Category
Hiplot

## When to Use
Create a Line (Color Dot) using R with the Hiplot platform's approach. Suitable for biomedical data visualization with publication-quality output.

## Required R Packages
- data.table
- grafify
- jsonlite

## Minimal Reproducible Code
```r
# Load packages
library(data.table)
library(grafify)
library(jsonlite)

# Prepare data
# Load data
data <- data.table::fread(jsonlite::read_json("https://hiplot.cn/ui/basic/line-color-dot/data.json")$exampleData[[1]]$textarea[[1]])
data <- as.data.frame(data)

# Convert data structure
x <- "Time"
y <- "PI"
group <- "Experiment"
facet <- "Genotype"
data[, x] <- factor(data[, x], levels = unique(data[, x]))
data[, group] <- factor(data[, group], levels = unique(data[, group]))
data[, facet] <- factor(data[, facet], levels = unique(data[, facet]))

# View data
head(data)

# Create visualization
# Line (Color Dot)
p <- plot_befafter_colours(
  data = data, xcol = get(x), ycol = get(y), match = get(group),
  symsize = 5, symthick = 1, s_alpha = 1) +
  facet_wrap(facet) +
  guides(fill = guide_legend(title = group)) +
  scale_fill_grafify() +
  xlab(x) + ylab(y) +
  ggtitle("Two-way repeated measures") +
  theme(text = element_text(family = "Arial"),
        plot.title = element_text(size = 12, hjust = 0.5),
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
- `alpha`: Controls transparency (0 = fully transparent, 1 = opaque)
- `position`: Position adjustment (identity, dodge, stack, fill)
- `fill`: Maps a variable to fill color for group comparison
- `color`: Maps a variable to outline/point color

## Tips
- Customize color scales with `scale_fill_manual()` or `scale_color_brewer()`
- Use `facet_wrap()` or `facet_grid()` to create multi-panel plots by group
- See the full tutorial for additional customization options and advanced examples

## Full Tutorial
https://openbiox.github.io/Bizard/Hiplot/092-line-color-dot.html
