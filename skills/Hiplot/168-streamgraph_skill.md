# Skill: Streamgraph (R)

## Category
Hiplot

## When to use
::: callout-note
**Hiplot website**

## Required R packages
- data.table
- jsonlite
- streamgraph

## Minimal reproducible code
```r
# Streamgraph
p <- streamgraph(data, key = "key", value = "value", date = "date",
                 offset = "silhouette", interpolate = "cardinal",
                 interactive = F, scale = "date") %>% 
  sg_fill_brewer(palette = "Spectral")

p
```

## Full tutorial
https://openbiox.github.io/Bizard/Hiplot/168-streamgraph.html
