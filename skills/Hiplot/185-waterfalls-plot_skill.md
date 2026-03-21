# Skill: Waterfalls Plot2 (R)

## Category
Hiplot

## When to Use
::: callout-note
**Hiplot website**

## Required R Packages
- data.table
- ggplot2
- jsonlite
- waterfalls

## Minimal Reproducible Code
```r
# Waterfalls Plot2
p <- waterfall(data, calc_total = T, rect_width = 0.7, fill_by_sign = F,
               fill_colours = data$fill, total_rect_color = "#1E065D") +
  theme_bw()

p
```

## Full Tutorial
https://openbiox.github.io/Bizard/Hiplot/185-waterfalls-plot.html
