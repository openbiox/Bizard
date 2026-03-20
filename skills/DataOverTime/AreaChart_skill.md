# Skill: Area Chart (R)

## Category
DataOverTime

## When to use
An area chart is a line chart in which the area below the line is filled with color. It is mainly used to display values at continuous intervals or over a time span.

## Required R packages
- dygraphs
- ggpattern
- hrbrthemes
- tidyverse
- viridis
- xts

## Minimal reproducible code
```r
# Basic area plot
p <- ggplot(monthly_death_counts, aes(x = month, y = deaths)) +
  geom_area() +
  labs(title = "Cumulative Deaths Over Time",
       x = "Months",
       y = "Number of Deaths")

p
```

## Full tutorial
https://openbiox.github.io/Bizard/DataOverTime/AreaChart.html
