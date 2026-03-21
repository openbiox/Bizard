# Skill: Pyramid Chart 2 (R)

## Category
Hiplot

## When to Use
The pyramid chart is a pyramid-like figure that distributes data on both sides of a central axis.

## Required R Packages
- apyramid
- data.table
- ggplot2
- jsonlite

## Minimal Reproducible Code
```r
# Load packages
library(apyramid)
library(data.table)
library(ggplot2)
library(jsonlite)

# Prepare data
# Load data
data <- data.table::fread(jsonlite::read_json("https://hiplot.cn/ui/basic/pyramid-chart2/data.json")$exampleData[[1]]$textarea[[1]])
data <- as.data.frame(data)

# Convert data structure
x <- as.integer(data[,"age"])
data$age_group <- cut(x, breaks = pretty(x), right = TRUE, include.lowest = TRUE)

# View data
head(data[,1:5])

# Create visualization
# Pyramid Chart 2
p <- age_pyramid(data, "age_group", split_by = "Gender") + 
  scale_fill_manual(values = c("#BC3C29FF","#0072B5FF")) +
  xlab("Age group") + 
  ylab("Gender") +
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
https://openbiox.github.io/Bizard/Hiplot/145-pyramid-chart2.html
