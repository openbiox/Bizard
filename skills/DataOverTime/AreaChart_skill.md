# Skill: Area Chart (R)

## Category
DataOverTime

## When to Use
An area chart is a line chart in which the area below the line is filled with color. It is mainly used to display values at continuous intervals or over a time span.

## Required R Packages
- dygraphs
- ggpattern
- hrbrthemes
- tidyverse
- viridis
- xts

## Minimal Reproducible Code
```r
# Basic area plot
p <- ggplot(monthly_death_counts, aes(x = month, y = deaths)) +
  geom_area() +
  labs(title = "Cumulative Deaths Over Time",
       x = "Months",
       y = "Number of Deaths")

p
```

## Full Tutorial
https://openbiox.github.io/Bizard/DataOverTime/AreaChart.html
