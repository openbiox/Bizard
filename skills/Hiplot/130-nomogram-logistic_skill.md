# Skill: Nomogram (Logistic) (R)

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

## Minimal reproducible code
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

## Full tutorial
https://openbiox.github.io/Bizard/Hiplot/130-nomogram-logistic.html
