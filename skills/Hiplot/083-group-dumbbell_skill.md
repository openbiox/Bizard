# Skill: Group Dumbbell (R)

## Category
Hiplot

## When to use
::: callout-note
**Hiplot website**

## Required R packages
- data.table
- ggalt
- ggplot2
- jsonlite

## Minimal reproducible code
```r
# Group Dumbbell
p <- ggplot(data = data, aes(x = y1952, xend = y2007, y = country, color = group)) +
  geom_dumbbell(size = 1, size_xend = 2, size_x = 2) +
  theme_bw()

p
```

## Full tutorial
https://openbiox.github.io/Bizard/Hiplot/083-group-dumbbell.html
