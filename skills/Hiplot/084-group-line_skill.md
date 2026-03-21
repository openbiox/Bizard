# Skill: Group Line (R)

## Category
Hiplot

## When to Use
::: callout-note
**Hiplot website**

## Required R Packages
- data.table
- ggplot2
- jsonlite

## Minimal Reproducible Code
```r
# Group Line
p <- ggplot(data, aes(x = x, y = y, group = names, color = groups)) +
  geom_line() +
  geom_point() +
  scale_color_manual(values = c("#e04d39","#5bbad6")) +
  theme_bw()

p
```

## Full Tutorial
https://openbiox.github.io/Bizard/Hiplot/084-group-line.html
