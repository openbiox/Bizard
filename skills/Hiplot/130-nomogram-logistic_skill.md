# Skill: Nomogram (Logistic) (R)

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

## Minimal Reproducible Code
```r
# Nomogram (Logistic)
p <- as.ggplot(function() {
  plot(logistic_nomo,
    scale = 1
  )
  title(main = "Nomogram (Logistic)")
})

p
```

## Full Tutorial
https://openbiox.github.io/Bizard/Hiplot/130-nomogram-logistic.html
