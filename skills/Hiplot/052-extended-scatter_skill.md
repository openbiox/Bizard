# Skill: Extended Scatter (R)

## Category
Hiplot

## When to Use
An extended scatter plot adds marginal plots to the basic scatter plot to provide a more comprehensive view of the data distribution.

## Required R Packages
- data.table
- ggExtra
- ggplot2
- jsonlite

## Minimal Reproducible Code
```r
# Load packages
library(data.table)
library(ggExtra)
library(ggplot2)
library(jsonlite)

# Prepare data
# Load data
data <- data.table::fread(jsonlite::read_json("https://hiplot.cn/ui/basic/extended-scatter/data.json")$exampleData$textarea[[1]])
data <- as.data.frame(data)

# View data
head(data)

# Create visualization
# Extended Scatter
p <- ggplot(data, aes(x = wt, y = mpg, color = cyl, size = cyl)) +
  geom_point() +
  geom_rug(alpha = 0.2, size = 1.5, col = "#4f80b3") +
  theme(legend.position = "none")

p <- ggMarginal(
  p, type = "densigram", fill = "#7054cc", color = "#7f0080",
  size = 4, bins = 30)

p
```

## Key Parameters
- `x`: Maps `wt` to the x aesthetic
- `y`: Maps `mpg` to the y aesthetic
- `color`: Maps `cyl` to the color aesthetic
- `size`: Maps `cyl` to the size aesthetic
- `alpha`: Controls transparency (0 = fully transparent, 1 = opaque)
- `position`: Position adjustment (identity, dodge, stack, fill)

## Tips
- Adjust text size with `theme(text = element_text(size = 14))` for presentations
- See the full tutorial for additional customization options and advanced examples

## Full Tutorial
https://openbiox.github.io/Bizard/Hiplot/052-extended-scatter.html
