# Skill: Line Chart (R)

## Category
DataOverTime

## When to use
Drawing line segments in various charts is common, and this module will draw all kinds of line segments that may be used.

## Required R packages
- dplyr
- gghighlight
- ggplot2
- ggpmisc
- patchwork
- viridis

## Minimal reproducible code
```r
# Basic Plotting
p <- ggplot(data, aes(x = Sepal.Length, y = Sepal.Width)) +
  geom_line()

p
```

## Full tutorial
https://openbiox.github.io/Bizard/DataOverTime/LineChart.html
