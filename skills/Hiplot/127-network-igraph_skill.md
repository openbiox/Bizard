# Skill: Network (igraph) (R)

## Category
Hiplot

## When to use
::: callout-note
**Hiplot website**

## Required R packages
- RColorBrewer
- data.table
- ggplotify
- igraph
- jsonlite
- stringr

## Minimal reproducible code
```r
# Network (igraph)
raw <- par()
p <- as.ggplot(function () {
  par(mar=c(8,2,2,2))
  radian.rescale <- function(x, start=0, direction=1) {
    c.rotate <- function(x) (x + start) %% (4 * pi) * direction
    c.rotate(scales::rescale(x, c(0, 2 * pi), range(x)))
  }

  label <- eval(parse(text = sprintf("V(net)$%s", "media")))

  l <- do.call(layout_as_star, list(net))
  params <- list(net, layout = l, main = "Network1",
      edge.color = edge.col, edge.curved = .1,
      vertex.shape = "circle",
      edge.lty = "solid",
      label.family = "Arial",
      vertex.label.family = "Arial",
      vertex.label.dist = 3.1,
      edge.arrow.mode = F
  )
  lab.locs <- radian.rescale(x=1:length(label), direction=-1, start=0)
  params$vertex.label.degree <- lab.locs
  params$vertex.label <- label
  params$vertex.color = V(net)$color
  do.call(plot, params)
  legend(x = -1.7, y = -1.4, unique(nodes_data[,"type.label"]), pch = 21,
        col = "#777777", pt.bg = colrs, pt.cex = 2, cex = .8, bty = "n",
        ncol = 1)
  legend(x = -1.2, y = -1.37,
    legend=round(sort(unique(E(net)$width)), 2), pt.cex= 0.8,
      col='black', ncol = 3, bty = "n", lty = 1,
      lwd = round(sort(unique(E(net)$width)), 2)
  )
  if (length(unique(V(net)$size)) > 8) {
    size_leg <- sort(unique(V(net)$size))[seq(1, length(unique(V(net)$size)), 2)]
  } else {
    size_leg <- sort(unique(V(net)$size))
  }
  legend(x = 0.5, y = -1.3,
        size_leg,
        pch = 21,
        col = "black", pt.bg = "#777777",
        pt.cex = size_leg / 3.8, cex = .8, bty = "n",
        ncol = 3,
        y.intersp = 3,
        x.intersp = 2.5,
        text.width = 0.25
  )
  par(mar=raw$mar)
})

p
```

## Full tutorial
https://openbiox.github.io/Bizard/Hiplot/127-network-igraph.html
