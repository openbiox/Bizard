# Skill: Scatterstats (R)

## Category
Hiplot

## When to use
::: callout-note
**Hiplot website**

## Required R packages
- data.table
- ggstatsplot
- jsonlite

## Minimal reproducible code
```r
# Scatterstats
p <- ggscatterstats(
  data = data, x = rating, y = budget
)

p
```

## Full tutorial
https://openbiox.github.io/Bizard/Hiplot/073-ggscatterstats.html
