# Skill: Cox Models Forest (R)

## Category
Hiplot

## When to use
::: callout-note
**Hiplot website**

## Required R packages
- data.table
- ezcox
- jsonlite

## Minimal reproducible code
```r
# Cox Models Forest
p <- show_forest(
  data = data,
  covariates = c("sex", "ph.ecog"),
  controls = "age",
  merge_models = F,
  drop_controls = F,
  add_caption = T
)

p
```

## Full tutorial
https://openbiox.github.io/Bizard/Hiplot/053-ezcox.html
