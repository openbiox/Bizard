# Skill: Beeswarm Plot (R)

## Category
Distribution

## When to use
A beeswarm plot disperses data points slightly to prevent overlap, making distribution density and trends clearer. It is especially useful for visualizing categorical data in small datasets. This section presents examples using R and the `beeswarm` and `ggbeeswarm` packages.

## Required R packages
- beeswarm
- ggbeeswarm
- ggsignif
- plyr
- readr
- tidyverse

## Minimal reproducible code
```r
p1 <- beeswarm(iris$Sepal.Length)
```

## Full tutorial
https://openbiox.github.io/Bizard/Distribution/Beeswarm.html
