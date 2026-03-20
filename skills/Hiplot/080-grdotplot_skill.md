# Skill: Group Rank Dotplot (R)

## Category
Hiplot

## When to use
::: callout-note
**Hiplot website**

## Required R packages
- data.table
- ggplot2
- jsonlite
- sigminer

## Minimal reproducible code
```r
# Group Rank Dotplot
p <- show_group_distribution(data, gvar = "gvar",  dvar = "dvar", 
                             order_by_fun = F)

p
```

## Full tutorial
https://openbiox.github.io/Bizard/Hiplot/080-grdotplot.html
