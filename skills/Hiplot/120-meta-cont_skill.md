# Skill: Meta-analysis of Continuous Data (R)

## Category
Hiplot

## When to Use
::: callout-note
**Hiplot website**

## Required R Packages
- data.table
- ggplotify
- jsonlite
- meta

## Minimal Reproducible Code
```r
# Meta-analysis of Continuous Data
p <- as.ggplot(function(){
  meta::forest(m1, layout = "meta")
  })

p
```

## Full Tutorial
https://openbiox.github.io/Bizard/Hiplot/120-meta-cont.html
