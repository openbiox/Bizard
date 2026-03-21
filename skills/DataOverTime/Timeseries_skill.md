# Skill: Timeseries (R)

## Category
DataOverTime

## When to Use
A time series graph is a statistical chart with time on the horizontal axis and the observed variable on the vertical axis, reflecting the trend of the observed variable over time.

## Required R Packages
- dplyr
- ggplot2
- patchwork

## Minimal Reproducible Code
```r
# Basic plot
p <- ggplot(data, aes(x = date, y = psavert)) +
  geom_line() +
  xlab("")

p
```

## Full Tutorial
https://openbiox.github.io/Bizard/DataOverTime/Timeseries.html
