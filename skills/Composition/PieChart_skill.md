# Skill: Pie Chart (R)

## Category
Composition

## When to Use
A pie chart is a basic chart in statistics, using sectors of different sizes to represent the magnitude of each item. A pie chart provides a visual understanding of the proportion of each data point within the overall data.

## Required R Packages
- ggplot2

## Minimal Reproducible Code
```r
# Load packages
library(ggplot2)

# Prepare data
# Data writing: one column for grouping, one column for values.
data <- data.frame(
  group = c("I", "II", "III", "IV", "NA"),
  value = c(402, 955, 1252, 3343, 3567)
)

head(data)

# Create visualization
# Basic drawing - bar chart
p <- ggplot(data, aes(x = "", y = value, fill = group)) +
  geom_col() # First, draw a bar chart, then transform it into a pie chart using coord_polar().

p
```

## Key Parameters
- `y`: Maps `value` to the y aesthetic
- `fill`: Maps `group` to the fill aesthetic
- `x`: Maps `rep_len` to the x aesthetic
- `position`: Position adjustment (identity, dodge, stack, fill)
- `theme`: Plot theme; tutorial uses `theme_void()`

## Tips
- Customize color scales with `scale_fill_manual()` or `scale_color_brewer()`
- Adjust text size with `theme(text = element_text(size = 14))` for presentations
- Ensure proportions sum to 100% and consider using a colorblind-friendly palette

## Full Tutorial
https://openbiox.github.io/Bizard/Composition/PieChart.html
