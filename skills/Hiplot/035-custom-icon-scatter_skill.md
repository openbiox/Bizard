# Skill: Custom Icon Scatter (R)

## Category
Hiplot

## When to use
::: callout-note
**Hiplot website**

## Required R packages
- data.table
- echarts4r
- echarts4r.assets
- jsonlite

## Minimal reproducible code
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

## Full tutorial
https://openbiox.github.io/Bizard/Hiplot/035-custom-icon-scatter.html
