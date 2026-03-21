# Skill: Moon charts (R)

## Category
Hiplot

## When to Use
::: callout-note
**Hiplot website**

## Required R Packages
- data.table
- gggibbous
- ggplot2
- jsonlite

## Minimal Reproducible Code
```r
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

## Full Tutorial
https://openbiox.github.io/Bizard/Hiplot/122-moon-charts.html
