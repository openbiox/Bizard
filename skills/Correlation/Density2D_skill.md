# Skill: 2D Density (R)

## Category
Correlation

## When to use
A 2D density plot shows the distribution of a combination of two numerical variables, using color gradients (or contour lines) to indicate the number of observations within an area. This can be used to identify trends in a dataset and analyze relationships between two variables. Scatter plots can be difficult to interpret when displaying large datasets because the points overlap and cannot be individually distinguished. In these cases, a two-dimensional density plot is useful.

## Required R packages
- MASS
- RColorBrewer
- ggplot2
- hexbin
- mvtnorm
- patchwork
- plotly

## Minimal reproducible code
```r
# 2D Histogram
p <- ggplot(data_tcga, aes(x = TP53, y = MDM2)) +
  geom_bin2d() +
  labs(fill = "Gene_expression\n(STAR_counts)") + 
  theme_bw()

p
```

## Full tutorial
https://openbiox.github.io/Bizard/Correlation/Density2D.html
