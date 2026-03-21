# Skill: Cox Models Forest (R)

## Category
Hiplot

## When to Use
::: callout-note
**Hiplot website**

## Required R Packages
- data.table
- ezcox
- jsonlite

## Minimal Reproducible Code
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

## Full Tutorial
https://openbiox.github.io/Bizard/Hiplot/053-ezcox.html
