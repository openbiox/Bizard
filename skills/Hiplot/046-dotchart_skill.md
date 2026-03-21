# Skill: Dotchart (R)

## Category
Hiplot

## When to Use
Sliding bead chart is a graph of beads sliding on a column. It is the superposition of bar chart and scatter chart.

## Required R Packages
- data.table
- ggpubr
- jsonlite

## Minimal Reproducible Code
```r
# Load packages
library(data.table)
library(ggpubr)
library(jsonlite)

# Prepare data
# Load data
data <- data.table::fread(jsonlite::read_json("https://hiplot.cn/ui/basic/dotchart/data.json")$exampleData$textarea[[1]])
data <- as.data.frame(data)

# View data
head(data)

# Create visualization
# Dotchart
p <- ggdotchart(data, x = "Name", y = "Value", group = "Group", color = "Group",
                rotate = T, sorting = "descending",
                y.text.col = F, add = "segments", dot.size = 2) +
  xlab("Name") +
  ylab("Value") +
  ggtitle("DotChart Plot") +
  scale_color_manual(values = c("#e04d39","#5bbad6","#1e9f86")) +
  theme_classic() +
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
- `theme`: Plot theme; tutorial uses `theme_classic()`
- `fill`: Maps a variable to fill color for group comparison
- `color`: Maps a variable to outline/point color

## Tips
- Customize color scales with `scale_fill_manual()` or `scale_color_brewer()`
- Adjust text size with `theme(text = element_text(size = 14))` for presentations
- See the full tutorial for additional customization options and advanced examples

## Full Tutorial
https://openbiox.github.io/Bizard/Hiplot/046-dotchart.html
