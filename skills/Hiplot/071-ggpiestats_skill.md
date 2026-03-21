# Skill: Piestats (R)

## Category
Hiplot

## When to Use
::: callout-note
**Hiplot website**

## Required R Packages
- data.table
- ggplot2
- ggstatsplot
- jsonlite

## Minimal Reproducible Code
```r
# Piestats
p <- ggpiestats(data = data, x = am, y = cyl,
                paired = F) +
  scale_fill_manual(values = c("#3B4992FF","#EE0000FF"))

p
```

## Full Tutorial
https://openbiox.github.io/Bizard/Hiplot/071-ggpiestats.html
