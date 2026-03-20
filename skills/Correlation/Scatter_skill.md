# Skill: Scatter Plot (R)

## Category
Correlation

## When to use
A scatter plot is a basic visualization chart used to represent the general trend of the dependent variable changing with the independent variable.

## Required R packages
- dplyr
- geomtextpath
- ggExtra
- ggplot2
- ggpmisc
- ggpubr
- plotly

## Minimal reproducible code
```r
# Basic plotting
p <- ggplot(data, aes(x = Sepal.Width, y = Sepal.Length)) +
  geom_point()

p
```

## Full tutorial
https://openbiox.github.io/Bizard/Correlation/Scatter.html
