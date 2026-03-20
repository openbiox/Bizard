# Skill: Pyramid Stack2 (R)

## Category
Hiplot

## When to use
::: callout-note
**Hiplot website**

## Required R packages
- data.table
- ggplotify
- jsonlite
- plotrix

## Minimal reproducible code
```r
# Pyramid Stack2
p <- as.ggplot(function() {
  cols <- c("#E64B35FF","#4DBBD5FF","#00A087FF","#3C5488FF")
  names(cols) <- colnames(dat_left)
  cols <- cols[1:ncol(dat_left)]
  pyramid.plot(dat_left, dat_right, labels = agegrps, unit = "Value",
               lxcol = cols, rxcol = cols,
               laxlab=as.numeric(c(0,10,20,30)), raxlab=as.numeric(c(0,10,20,30)),
               top.labels=c(split_var[1], colnames(data)[1], split_var[2]),
               gap=4, ppmar=c(4,2,4,7), do.first="plot_bg(\"#FFFFFF\")")
  mtext("Porridge temperature by age and sex of bear", 3, 2, cex=1)
  legend("right", inset=c(-0.25,0), legend = colnames(dat_left), fill = cols)
  })

p
```

## Full tutorial
https://openbiox.github.io/Bizard/Hiplot/147-pyramid-stack2.html
