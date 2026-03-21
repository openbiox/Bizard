# Skill: Fan Plot (R)

## Category
Hiplot

## When to Use
::: callout-note
**Hiplot website**

## Required R Packages
- data.table
- ggplotify
- jsonlite
- plotrix

## Minimal Reproducible Code
```r
# Fan Plot
p <- as.ggplot(function() {
  fan.plot(data[, 2], main = "", labels = as.character(data[, 1]),
           col = c("#E64B35FF","#4DBBD5FF","#00A087FF","#3C5488FF"))
  })

p
```

## Full Tutorial
https://openbiox.github.io/Bizard/Hiplot/054-fan.html
