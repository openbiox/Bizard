# Skill: Streamgraph (R)

## Category
Hiplot

## When to Use
::: callout-note
**Hiplot website**

## Required R Packages
- data.table
- jsonlite
- streamgraph

## Minimal Reproducible Code
```r
# Streamgraph
p <- streamgraph(data, key = "key", value = "value", date = "date",
                 offset = "silhouette", interpolate = "cardinal",
                 interactive = F, scale = "date") %>% 
  sg_fill_brewer(palette = "Spectral")

p
```

## Full Tutorial
https://openbiox.github.io/Bizard/Hiplot/168-streamgraph.html
