# Skill: Extended Scatter (R)

## Category
Hiplot

## When to Use
::: callout-note
**Hiplot website**

## Required R Packages
- data.table
- ggExtra
- ggplot2
- jsonlite

## Minimal Reproducible Code
```r
# Extended Scatter
p <- ggplot(data, aes(x = wt, y = mpg, color = cyl, size = cyl)) +
  geom_point() +
  geom_rug(alpha = 0.2, size = 1.5, col = "#4f80b3") +
  theme(legend.position = "none")

p <- ggMarginal(
  p, type = "densigram", fill = "#7054cc", color = "#7f0080",
  size = 4, bins = 30)

p
```

## Full Tutorial
https://openbiox.github.io/Bizard/Hiplot/052-extended-scatter.html
