# Skill: 3D Barplot (R)

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
# 3D Barplot
p <- as.ggplot(function() {
  hist3D(
    x = 1:nrow(mat), y = seq_len(ncol(mat)), z = mat,
    bty = "g", phi = 20,
    theta = -55,
    xlab = colnames(data)[2],
    ylab = colnames(data)[3], zlab = colnames(data)[1],
    main = "3D Bar Plot", colkey = F,
    border = "black", shade = 0.8, axes = T,
    ticktype = "detailed", space = 0.3, d = 2, cex.axis = 0.3,
    colvar = as.numeric(as.factor(data[, 2])), alpha = 1,
    col = c("#E64B35FF","#4DBBD5FF","#00A087FF","#3C5488FF")
  )

  # Use text3D to label x axis
  text3D(
    x = 1:nrow(mat), y = rep(0.5, nrow(mat)), z = rep(3, nrow(mat)),
    labels = rownames(mat),
    add = TRUE, adj = 0, cex = 0.8
  )
  # Use text3D to label y axis
  text3D(
    x = rep(1, ncol(mat)), y = seq_len(ncol(mat)), z = rep(0, ncol(mat)),
    labels = colnames(mat), bty = "g",
    add = TRUE, adj = 1, cex = 0.8
  )
})

p
```

## Full tutorial
https://openbiox.github.io/Bizard/Hiplot/003-barplot-3d.html
