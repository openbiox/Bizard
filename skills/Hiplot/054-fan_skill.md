# Skill: Fan Plot (R)

## Category
Hiplot

## When to use
::: callout-note
**Hiplot website**

## Required R packages
- data.table
- ggplotify
- jsonlite
- plotrix

## Minimal reproducible code
```r
# Fan Plot
p <- as.ggplot(function() {
  fan.plot(data[, 2], main = "", labels = as.character(data[, 1]),
           col = c("#E64B35FF","#4DBBD5FF","#00A087FF","#3C5488FF"))
  })

p
```

## Full tutorial
https://openbiox.github.io/Bizard/Hiplot/054-fan.html
