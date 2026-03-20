# Skill: Funnel Plot (metafor) (R)

## Category
Hiplot

## When to use
::: callout-note
**Hiplot website**

## Required R packages
- data.table
- ggplotify
- jsonlite
- metafor

## Minimal reproducible code
```r
# Funnel Plot
p <- as.ggplot(function(){
  funnel(x = res, main = "Funnel Plot (metafor)",
         level = c(90, 95, 99), shade = c("white","#a90e07","#d23e0b"), refline = 0)
  })

p
```

## Full tutorial
https://openbiox.github.io/Bizard/Hiplot/057-funnel-plot-metafor.html
