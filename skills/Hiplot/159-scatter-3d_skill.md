# Skill: 3D-Scatter (R)

## Category
Hiplot

## When to use
::: callout-note
**Hiplot website**

## Required R packages
- data.table
- ggplotify
- jsonlite
- plot3D

## Minimal reproducible code
```r
# 3D-Scatter
p <- as.ggplot(function() {
  plot3d <- scatter3D(data[, 1], data[, 2], data[, 3],
    pch = shapes, cex = 1,
    phi = 0, theta = 45, ticktype = "detailed",
    bty = "b2", colkey = FALSE, alpha = 1,
    xlab = colnames(data)[1], ylab = colnames(data)[2],
    zlab = colnames(data)[3],
    main = "3D-Scatter Plot",
    colvar = as.numeric(as.factor(data[, 4])),
    col = c("#e04d39","#5bbad6","#1e9f86")
  )
  
  legend("right", pch=19, legend = levels(data[, col_idx]),
         cex = 1.1, bty = 'n', xjust = 0.5, horiz = F,
         title = colnames(data)[col_idx],
         col = c("#e04d39","#5bbad6","#1e9f86"))
})

p
```

## Full tutorial
https://openbiox.github.io/Bizard/Hiplot/159-scatter-3d.html
