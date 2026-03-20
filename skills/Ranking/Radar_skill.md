# Skill: Radar/Spider Plot (R)

## Category
Ranking

## When to use
A radar chart, spider chart, or web chart is a two-dimensional chart type used to plot a series of values over one or more quantitative variables. The fmsb library is an excellent tool for building this type of chart in R.

## Required R packages
- fmsb

## Minimal reproducible code
```r
# Data collation
iris_setosa <- iris[c(1:50),]
iris_setosa <- iris_setosa[,-5]
iris_setosa_radar <- rbind(rep(6,4),rep(0,4),iris_setosa)
# plot
par(mar = c(1, 1, 1, 1))
radarchart(iris_setosa_radar)
```

## Full tutorial
https://openbiox.github.io/Bizard/Ranking/Radar.html
