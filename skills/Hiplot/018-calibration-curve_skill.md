# Skill: Calibration Curve (R)

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

## Full Tutorial
https://openbiox.github.io/Bizard/Hiplot/018-calibration-curve.html
