# Skill: PCA Plot (R)

## Category
Correlation

## When to use
Visualize pca plot data in a biomedical context.

## Required R packages
- FactoMineR
- dplyr
- factoextra
- ggfortify
- ggplot2

## Minimal reproducible code
```r
fviz_eig(iris.pca, 
         addlabels = TRUE, 
         ylim = c(0, 85),
         main = "PCA variance explained proportion",
         xlab = "PC",
         ylab = "Percentage of variance explained")
```

## Full tutorial
https://openbiox.github.io/Bizard/Correlation/PCAplot.html
