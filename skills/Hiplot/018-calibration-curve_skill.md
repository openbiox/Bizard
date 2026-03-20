# Skill: Calibration Curve (R)

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
# Calibration Curve
p <- as.ggplot(function() {
  plot(lrm.cal,
       xlab = "Nomogram Predicted Survival",
       ylab = "Actual Survival",
       main = "Calibration Curve"
       )
})

p
```

## Full tutorial
https://openbiox.github.io/Bizard/Hiplot/018-calibration-curve.html
