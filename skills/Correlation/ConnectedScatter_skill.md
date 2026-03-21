# Skill: Connected Scatter (R)

## Category
Correlation

## When to Use
Connected scatter is a type of chart that builds upon scatter by adding lines to connect the data points in a certain order. It allows us to discern not only the correlation between independent variable and dependent variable but also the trend in the data points.

## Required R Packages
- dplyr
- ggplot2
- stringr

## Minimal Reproducible Code
```r
# Basic plotting, only adding `geom_line`
p <- ggplot(data[data$Species == "setosa", ], aes(x = Sepal.Width, y = Sepal.Length)) +
  geom_point(shape = 17, size = 1.5, color = "blue") +
  geom_line()

p
```

## Full Tutorial
https://openbiox.github.io/Bizard/Correlation/ConnectedScatter.html
