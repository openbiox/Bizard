# Skill: Nomogram (R)

## Category
Hiplot

## When to use
::: callout-note
**Hiplot website**

## Required R packages
- data.table
- ggplotify
- jsonlite
- rms
- survival

## Minimal reproducible code
```r
# Nomogram
p <- as.ggplot(function() {
  plot(cox_nomo, scale = 1)
  title(main = "Nomogram (COX)")
})

p
```

## Full tutorial
https://openbiox.github.io/Bizard/Hiplot/131-nomogram.html
