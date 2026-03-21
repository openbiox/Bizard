# Skill: Histostats (R)

## Category
Hiplot

## When to Use
Display data distribution and inference.

## Required R Packages
- data.table
- ggstatsplot
- jsonlite

## Minimal Reproducible Code
```r
# Load packages
library(data.table)
library(ggstatsplot)
library(jsonlite)

# Prepare data
# Load data
data <- data.table::fread(jsonlite::read_json("https://hiplot.cn/ui/basic/gghistostats/data.json")$exampleData$textarea[[1]])
data <- as.data.frame(data)

# Convert data structure
axis <- c("budget", "genre")
data[, axis[2]] <- factor(data[, axis[2]], levels = unique(data[, axis[2]]))

# View data
head(data)

# Create visualization
# Histostats
p <- grouped_gghistostats(
  data = data, x = budget, grouping.var = genre,
  effsize.type = "unbiased",
  type = "parametric",
  centrality.k = 2,
  plotgrid.args = list(ncol = 2),
  centrality.parameter = "solid",
  centrality.line.args = list(size = 1, color = "black"),
  bar.fill = "#0D47A1", 
  centrality.label.args = list(color = "#0D47A1", size = 3),
  test.value = as.numeric(0),
  normal.curve = F,
  normal.curve.args = list(size = 1)
)

p
```

## Key Parameters
- `stat`: Statistical transformation to use
- `fill`: Maps a variable to fill color for group comparison
- `color`: Maps a variable to outline/point color

## Tips
- Adjust text size with `theme(text = element_text(size = 14))` for presentations
- See the full tutorial for additional customization options and advanced examples

## Full Tutorial
https://openbiox.github.io/Bizard/Hiplot/067-gghistostats.html
