# Skill: Circular Packing Chart (R)

## Category
Composition

## When to use
Circular Packing can be viewed as a special type of classification tree diagram, which is particularly suitable for displaying classification data with hierarchical relationships.

## Required R packages
- circlepackeR
- cowplot
- data.tree
- dplyr
- flare
- ggiraph
- ggplot2
- ggraph
- htmlwidgets
- igraph
- packcircles
- tidyr
- tidyverse
- viridis

## Minimal reproducible code
```r
## color
data_BP1 <- data_BP
data_BP1$pvalue_log <- -log10(data_BP1$pvalue)
packing_BP <- circleProgressiveLayout(data_BP1$pvalue_log, sizetype='area' )


# Merging plotting data
data_BUBBLE_BP <- cbind(data_BP1, packing_BP)

# Generate the coordinates of each vertex of the circle, where npoint is the number of vertices.
dat.gg_BP <- circleLayoutVertices(packing_BP, npoints=50)

# plot
p <- ggplot() + 
  geom_polygon(data = dat.gg_BP, 
               aes(x, y, group = id, fill=as.factor(id)), 
               colour = "black", alpha = 0.6) +
  scale_fill_manual(values = magma(nrow(data_BUBBLE_BP))) + # change color
  geom_text(data = data_BUBBLE_BP,
            aes(x, y, size=pvalue_log, label = str_wrap(BP,width = 10)),
            show.legend = FALSE) +
  scale_size_continuous(range = c(0.5,1.5)) +
  theme_void() + 
  theme(legend.position="none",
        plot.title = element_text(hjust=0.5,size = 20)) +
  coord_equal() +
  ggtitle("Basic circular packing chart + custom colors")

p
```

## Full tutorial
https://openbiox.github.io/Bizard/Composition/CircularPacking.html
