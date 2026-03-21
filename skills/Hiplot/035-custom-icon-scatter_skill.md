# Skill: Custom Icon Scatter (R)

## Category
Hiplot

## When to Use
::: callout-note
**Hiplot website**

## Required R Packages
- data.table
- echarts4r
- echarts4r.assets
- jsonlite

## Minimal Reproducible Code
```r
# Custom Icon Scatter
p <- draw_data |>
  e_charts(x) |>
  e_scatter(
    y,
    size,
    symbol = ea_icons("warning"),
    name = "warning"
    )

p
```

## Full Tutorial
https://openbiox.github.io/Bizard/Hiplot/035-custom-icon-scatter.html
