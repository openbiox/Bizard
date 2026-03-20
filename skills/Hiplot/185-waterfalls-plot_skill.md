# Skill: Waterfalls Plot2 (R)

## Category
Hiplot

## When to use
::: callout-note
**Hiplot website**

## Required R packages
- data.table
- ggplot2
- jsonlite
- waterfalls

## Minimal reproducible code
```r
# Waterfalls Plot2
p <- waterfall(data, calc_total = T, rect_width = 0.7, fill_by_sign = F,
               fill_colours = data$fill, total_rect_color = "#1E065D") +
  theme_bw()

p
```

## Full tutorial
https://openbiox.github.io/Bizard/Hiplot/185-waterfalls-plot.html
