# Skill: Ridgeline Plot (R)

## Category
Distribution

## When to use
A ridgeline plot, also known as a joyplot, visualizes the distribution of multiple numeric variables across different categories. This method is useful for comparing density distributions while preserving an overall view of trends and variations.

## Required R packages
- dplyr
- ggplot2
- ggridges
- hrbrthemes
- readr
- viridis

## Minimal reproducible code
```r
# Basic Ridgeline plot
p1_1 <- ggplot(iris, aes(x = Sepal.Length, y = Species, fill = Species)) +
  geom_density_ridges(alpha = 0.5) +
  theme_ridges(font_size = 16, grid = TRUE) +
  theme(legend.position = "right")

p1_1
```

## Full tutorial
https://openbiox.github.io/Bizard/Distribution/Ridgeline.html
