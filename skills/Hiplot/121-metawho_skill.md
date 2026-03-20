# Skill: Meta-Subgroup Analysis (R)

## Category
Hiplot

## When to use
::: callout-note
**Hiplot website**

## Required R packages
- cowplot
- data.table
- jsonlite
- metawho

## Minimal reproducible code
```r
# Meta-Subgroup Analysis
p1 <- deft_show(res, element = "all")
p2 <- deft_show(res, element = "subgroup")
p <- plot_grid(p1, p2, nrow = 2)

p
```

## Full tutorial
https://openbiox.github.io/Bizard/Hiplot/121-metawho.html
