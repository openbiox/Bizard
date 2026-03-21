# Skill: Nomogram (R)

## Category
Hiplot

## When to Use
::: callout-note
**Hiplot website**

## Required R Packages
- data.table
- ggplotify
- jsonlite
- rms
- survival

## Minimal Reproducible Code
```r
# Nomogram
p <- as.ggplot(function() {
  plot(cox_nomo, scale = 1)
  title(main = "Nomogram (COX)")
})

p
```

## Full Tutorial
https://openbiox.github.io/Bizard/Hiplot/131-nomogram.html
