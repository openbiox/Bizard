# Skill: Treeheatr (R)

## Category
Hiplot

## When to use
::: callout-note
**Hiplot website**

## Required R packages
- data.table
- ggplotify
- jsonlite
- treeheatr

## Minimal reproducible code
```r
# Treeheatr
p <- as.ggplot(function() {
  print(heat_tree(data,
    target_lab = "species",
    task = 'classification',
    show = "heat-tree",
    heat_rel_height = 0.2,
    panel_space = 0.001,
    clust_samps = T,
    clust_target = T,
    lev_fac = 1.3,
    cont_legend = F,
    cate_legend = F
  ))
})

p
```

## Full tutorial
https://openbiox.github.io/Bizard/Hiplot/172-treeheatr.html
