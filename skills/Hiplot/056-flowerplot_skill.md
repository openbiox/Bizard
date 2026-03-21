# Skill: Flower plot (R)

## Category
Hiplot

## When to Use
::: callout-note
**Hiplot website**

## Required R Packages
- data.table
- flowerplot
- ggplotify
- jsonlite

## Minimal Reproducible Code
```r
# Flower plot
p <- as.ggplot(function(){
  flowerplot(
    flower_dat = data,
    angle = 90,
    a = 0.5,
    b = 2,
    r = 1,
    ellipse_col = "RdBu",
    circle_col = "#FFFFFF",
    label_text_cex = 1
  )})

p
```

## Full Tutorial
https://openbiox.github.io/Bizard/Hiplot/056-flowerplot.html
