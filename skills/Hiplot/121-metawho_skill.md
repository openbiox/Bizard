# Skill: Meta-Subgroup Analysis (R)

## Category
Hiplot

## When to Use
::: callout-note
**Hiplot website**

## Required R Packages
- cowplot
- data.table
- jsonlite
- metawho

## Minimal Reproducible Code
```r
# Meta-Subgroup Analysis
p1 <- deft_show(res, element = "all")
p2 <- deft_show(res, element = "subgroup")
p <- plot_grid(p1, p2, nrow = 2)

p
```

## Full Tutorial
https://openbiox.github.io/Bizard/Hiplot/121-metawho.html
