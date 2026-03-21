# Skill: Group Rank Dotplot (R)

## Category
Hiplot

## When to Use
::: callout-note
**Hiplot website**

## Required R Packages
- data.table
- ggplot2
- jsonlite
- sigminer

## Minimal Reproducible Code
```r
# Group Rank Dotplot
p <- show_group_distribution(data, gvar = "gvar",  dvar = "dvar", 
                             order_by_fun = F)

p
```

## Full Tutorial
https://openbiox.github.io/Bizard/Hiplot/080-grdotplot.html
