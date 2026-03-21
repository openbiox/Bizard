# Skill: Beeswarm Plot (R)

## Category
Distribution

## When to Use
A beeswarm plot disperses data points slightly to prevent overlap, making distribution density and trends clearer. It is especially useful for visualizing categorical data in small datasets. This section presents examples using R and the `beeswarm` and `ggbeeswarm` packages.

## Required R Packages
- beeswarm
- ggbeeswarm
- ggsignif
- plyr
- readr
- tidyverse

## Minimal Reproducible Code
```r
p1 <- beeswarm(iris$Sepal.Length)
```

## Full Tutorial
https://openbiox.github.io/Bizard/Distribution/Beeswarm.html
