# Skill: Histostats (R)

## Category
Hiplot

## When to Use
::: callout-note
**Hiplot website**

## Required R Packages
- data.table
- ggstatsplot
- jsonlite

## Minimal Reproducible Code
```r
# Histostats
p <- grouped_gghistostats(
  data = data, x = budget, grouping.var = genre,
  effsize.type = "unbiased",
  type = "parametric",
  centrality.k = 2,
  plotgrid.args = list(ncol = 2),
  centrality.parameter = "solid",
  centrality.line.args = list(size = 1, color = "black"),
  bar.fill = "#0D47A1", 
  centrality.label.args = list(color = "#0D47A1", size = 3),
  test.value = as.numeric(0),
  normal.curve = F,
  normal.curve.args = list(size = 1)
)

p
```

## Full Tutorial
https://openbiox.github.io/Bizard/Hiplot/067-gghistostats.html
