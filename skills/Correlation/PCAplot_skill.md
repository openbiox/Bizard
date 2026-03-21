# Skill: PCA Plot (R)

## Category
Correlation

## When to Use
Visualize pca plot data in a biomedical context.

## Required R Packages
- FactoMineR
- dplyr
- factoextra
- ggfortify
- ggplot2

## Minimal Reproducible Code
```r
fviz_eig(iris.pca, 
         addlabels = TRUE, 
         ylim = c(0, 85),
         main = "PCA variance explained proportion",
         xlab = "PC",
         ylab = "Percentage of variance explained")
```

## Full Tutorial
https://openbiox.github.io/Bizard/Correlation/PCAplot.html
