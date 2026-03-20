# Skill: Dendrogram (R)

## Category
Hiplot

## When to use
::: callout-note
**Hiplot website**

## Required R packages
- ape
- data.table
- ggplotify
- jsonlite

## Minimal reproducible code
```r
# Dendrogram
d <- dist(t(data), method = "euclidean")
hc <- hclust(d, method = "complete")
clus <- cutree(hc, 4)

p <- as.ggplot(function() {
  par(mar = c(5, 5, 10, 5), mgp = c(2.5, 1, 0))
  plot(as.phylo(hc),
       type = "phylogram",
       tip.color = c("#00468bff","#ed0000ff","#42b540ff","#0099b4ff")[clus], 
       label.offset = 1,
       cex = 1, font = 2, use.edge.length = T
       )
  title("Dendrogram Plot", line = 1)
  })

p
```

## Full tutorial
https://openbiox.github.io/Bizard/Hiplot/037-dendrogram.html
