# Skill: PCA Plot (R)

## Category
Correlation

## When to Use
Create a PCA Plot visualization in R for biomedical data analysis and research publications.

## Required R Packages
- FactoMineR
- dplyr
- factoextra
- ggfortify
- ggplot2

## Minimal Reproducible Code
```r
# Load packages
library(FactoMineR)
library(dplyr)
library(factoextra)
library(ggfortify)
library(ggplot2)

# Prepare data
data("iris")
head(iris)

# Create visualization
fviz_eig(iris.pca, 
         addlabels = TRUE, 
         ylim = c(0, 85),
         main = "PCA variance explained proportion",
         xlab = "PC",
         ylab = "Percentage of variance explained")
```

## Key Parameters
- `x`: Maps `PC1` to the x aesthetic
- `y`: Maps `PC2` to the y aesthetic
- `color`: Maps `Species` to the color aesthetic
- `alpha`: Controls transparency (0 = fully transparent, 1 = opaque)
- `width`: Controls element width
- `position`: Position adjustment (identity, dodge, stack, fill)
- `stat`: Statistical transformation to use

## Tips
- Use `theme_minimal()` or `theme_bw()` for clean, publication-ready plots
- Customize color scales with `scale_fill_manual()` or `scale_color_brewer()`
- Always check and report the correlation coefficient and p-value alongside visual patterns

## Full Tutorial
https://openbiox.github.io/Bizard/Correlation/PCAplot.html
