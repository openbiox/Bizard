# Skill: Scatterstats (R)

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
# Scatterstats
p <- ggscatterstats(
  data = data, x = rating, y = budget
)

p
```

## Full Tutorial
https://openbiox.github.io/Bizard/Hiplot/073-ggscatterstats.html
