# Skill: Bubble Plot (R)

## Category
Correlation

## When to Use
A bubble plot is a scatter plot in which a third numeric variable is mapped to the size of the circles. This article shows several ways to build bubble charts using R.

## Required R Packages
- dplyr
- gapminder
- ggplot2
- hrbrthemes
- viridis

## Minimal Reproducible Code
```r
# Taking iris data as an example
p <- ggplot(iris, aes(x=Sepal.Length, y=Sepal.Width, size = Species)) +
  geom_point(alpha=0.4)

p
```

## Full Tutorial
https://openbiox.github.io/Bizard/Correlation/Bubble.html
