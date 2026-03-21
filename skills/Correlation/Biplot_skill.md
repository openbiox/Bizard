# Skill: Biplot (R)

## Category
Correlation

## When to Use
Create a Biplot visualization in R for biomedical data analysis and research publications.

## Required R Packages
- dplyr
- ggbiplot

## Minimal Reproducible Code
```r
# Load packages
library(dplyr)
library(ggbiplot)

# Prepare data
data("iris")
head(iris)

# Create visualization
iris.gg <-
  ggbiplot(iris.pca, obs.scale = 1, var.scale = 1,
           groups = iris$Species, point.size=2,
           varname.size = 3, 
           varname.color = "black",
           varname.adjust = 1.2,
           ellipse = TRUE, 
           circle = TRUE) +
  labs(fill = "Species", color = "Species") +
  theme_minimal(base_size = 14) +
  theme(legend.direction = 'horizontal', legend.position = 'top')

iris.gg
```

## Key Parameters
- `x`: Maps `xvar` to the x aesthetic
- `y`: Maps `yvar` to the y aesthetic
- `position`: Position adjustment (identity, dodge, stack, fill)

## Tips
- Use `theme_minimal()` or `theme_bw()` for clean, publication-ready plots
- Adjust text size with `theme(text = element_text(size = 14))` for presentations
- Always check and report the correlation coefficient and p-value alongside visual patterns

## Full Tutorial
https://openbiox.github.io/Bizard/Correlation/Biplot.html
