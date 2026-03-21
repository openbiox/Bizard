# Skill: GGPIE (R)

## Category
Hiplot

## When to Use
The pie chart is a statistical chart that shows the proportion of each part by dividing a circle into sections.

## Required R Packages
- cowplot
- data.table
- dplyr
- ggpie
- ggplot2
- jsonlite

## Minimal Reproducible Code
```r
# Load packages
library(cowplot)
library(data.table)
library(dplyr)
library(ggpie)
library(ggplot2)
library(jsonlite)

# Prepare data
# Load data
data <- data.table::fread(jsonlite::read_json("https://hiplot.cn/ui/basic/ggpie/data.json")$exampleData$textarea[[1]])
data <- as.data.frame(data)

# Convert data structure
axis <- c("am", "cyl")
data[, axis[1]] <- factor(data[, axis[1]], levels = unique(data[, axis[1]]))
data[, axis[2]] <- factor(data[, axis[2]], levels = unique(data[, axis[2]]))

# View data
head(data)

# Create visualization
# GGPIE
plist <- list()
for (j in unique(data[, axis[2]])) {
  plist[[j]] <- ggpie(
    data = data[data[, axis[2]] == j,],
    group_key = axis[1], count_type = "full",
    label_type = "horizon", label_size = 8,
    label_info = "all", label_pos = "out") + 
    scale_fill_manual(values = c("#00468BFF","#ED0000FF")) +
    ggtitle(j)
  }

plot_grid(plotlist = plist, ncol = 3)
```

## Key Parameters
- `fill`: Maps a variable to fill color for group comparison
- `color`: Maps a variable to outline/point color

## Tips
- Customize color scales with `scale_fill_manual()` or `scale_color_brewer()`
- Adjust text size with `theme(text = element_text(size = 14))` for presentations
- See the full tutorial for additional customization options and advanced examples

## Full Tutorial
https://openbiox.github.io/Bizard/Hiplot/068-ggpie.html
