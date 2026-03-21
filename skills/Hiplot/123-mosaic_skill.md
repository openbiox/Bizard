# Skill: Mosaic Ratio Plot (R)

## Category
Hiplot

## When to Use
::: callout-note
**Hiplot website**

## Required R Packages
- DescTools
- data.table
- ggplotify
- jsonlite
- vcd

## Minimal Reproducible Code
```r
# Mosaic Ratio Plot
p <- as.ggplot(function() {
  mosaic(tbl, shade = TRUE, legend = TRUE, main = "Mosaic Ratio Plot",
         gp = shading_binary(tbl, col = c("#3B4992FF","#EE0000FF")))
})

p
```

## Full Tutorial
https://openbiox.github.io/Bizard/Hiplot/123-mosaic.html
