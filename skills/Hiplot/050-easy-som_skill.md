# Skill: Easy SOM (R)

## Category
Hiplot

## When to use
::: callout-note
**Hiplot website**

## Required R packages
- data.table
- jsonlite
- kohonen

## Minimal reproducible code
```r
# Easy SOM
p <- function () {
  par(mfrow = c(3,2))
  xyfpredictions <- classmat2classvec(getCodes(kohmap, 2))
  plot(kohmap, type="counts", col = as.integer(target),
       palette.name = colors,
       pchs = as.integer(target), 
       main = "Counts plot", shape = "straight", border = NA)
  
  som.hc <- cutree(hclust(object.distances(kohmap, "codes")), 3)
  add.cluster.boundaries(kohmap, som.hc)

  plot(kohmap, type="mapping",
       labels = as.integer(target), col = colors(3)[as.integer(target)],
       palette.name = colors,
       shape = "straight",
       main = "Mapping plot")

  ## add background colors to units according to their predicted class labels
  xyfpredictions <- classmat2classvec(getCodes(kohmap, 2))
  bgcols <- colors(3)
  plot(kohmap, type="mapping", col = as.integer(target),
       pchs = as.integer(target), bgcol = bgcols[as.integer(xyfpredictions)],
       main = "Another mapping plot", shape = "straight", border = NA)
  
  similarities <- plot(kohmap, type="quality", shape = "straight",
                       palette.name = colors)
  
  plot(kohmap, type="codes", shape = "straight", 
       main = c("Codes X", "Codes Y"), palette.name = colors)
}

p()
```

## Full tutorial
https://openbiox.github.io/Bizard/Hiplot/050-easy-som.html
