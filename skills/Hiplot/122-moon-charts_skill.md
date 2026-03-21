# Skill: Moon charts (R)

## Category
Hiplot

## When to Use
The moon chart is a graph that uses the moon's waxing and waning to reflect the size of the data.

## Required R Packages
- data.table
- gggibbous
- ggplot2
- jsonlite

## Minimal Reproducible Code
```r
# Load packages
library(data.table)
library(gggibbous)
library(ggplot2)
library(jsonlite)

# Prepare data
# Load data
data <- data.table::fread(jsonlite::read_json("https://hiplot.cn/ui/basic/moon-charts/data.json")$exampleData$textarea[[1]])
data <- as.data.frame(data)

# Convert data structure
data[, 1] <- factor(data[, 1], levels = unique(data[, 1]))
rest_cols <- colnames(data)[-1]
tidyrest <- reshape(
  data,
  varying = rest_cols,
  v.names = "Score",
  timevar = "Category",
  times = factor(rest_cols, levels = rest_cols),
  idvar = colnames(data)[1],
  direction = "long"
)

# View data
head(data)

# Create visualization
# Moon charts
p <- ggplot(tidyrest, aes(0, 0)) +
  geom_moon(aes(ratio = (Score - 1) / 4), fill = "black") +
  geom_moon(aes(ratio = 1 - (Score - 1) / 4), right = FALSE) +
  facet_grid(Category ~ Restaurant, switch = "y") +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    axis.text = element_blank(),
    axis.title = element_blank()
  )

p
```

## Key Parameters
- `theme`: Plot theme; tutorial uses `theme_minimal()`
- `fill`: Maps a variable to fill color for group comparison
- `color`: Maps a variable to outline/point color

## Tips
- Use `theme_minimal()` or `theme_bw()` for clean, publication-ready plots
- Use `facet_wrap()` or `facet_grid()` to create multi-panel plots by group
- See the full tutorial for additional customization options and advanced examples

## Full Tutorial
https://openbiox.github.io/Bizard/Hiplot/122-moon-charts.html
