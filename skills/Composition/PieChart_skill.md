# Skill: Pie Chart (R)

## Category
Composition

## When to Use
A pie chart is a basic chart in statistics, using sectors of different sizes to represent the magnitude of each item. A pie chart provides a visual understanding of the proportion of each data point within the overall data.

## Required R Packages
- ggplot2

## Minimal Reproducible Code
```r
# Basic drawing - bar chart
p <- ggplot(data, aes(x = "", y = value, fill = group)) +
  geom_col() # First, draw a bar chart, then transform it into a pie chart using coord_polar().

p
```

## Full Tutorial
https://openbiox.github.io/Bizard/Composition/PieChart.html
