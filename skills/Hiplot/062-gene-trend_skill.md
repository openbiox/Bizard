# Skill: Gene Cluster Trend (R)

## Category
Hiplot

## When to Use
::: callout-note
**Hiplot website**

## Required R Packages
- Mfuzz
- RColorBrewer
- data.table
- ggplotify
- jsonlite

## Minimal Reproducible Code
```r
# Gene Cluster Trend
p <- as.ggplot(function(){
  mfuzz.plot2(
  eset,
  cl,
  xlab = "Time",
  ylab = "Expression changes",
  mfrow = c(2,(c/2+0.5)),
  colo = "fancy",
  centre = T,
  centre.col = "red",
  time.labels = colnames(eset),
  x11=F)
  })

p
```

## Full Tutorial
https://openbiox.github.io/Bizard/Hiplot/062-gene-trend.html
