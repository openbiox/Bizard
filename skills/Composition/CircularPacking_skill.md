# Skill: Circular Packing Chart (R)

## Category
Composition

## When to Use
Circular Packing can be viewed as a special type of classification tree diagram, which is particularly suitable for displaying classification data with hierarchical relationships.

## Required R Packages
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

## Minimal Reproducible Code
```r
# Load packages
library(circlepackeR)
library(cowplot)
library(data.tree)
library(dplyr)
library(flare)
library(ggiraph)

# Prepare data
#GO BP
data_BP <- readr::read_csv("https://bizard-1301043367.cos.ap-guangzhou.myqcloud.com/data_BP.csv")

#flare
data_edges <- flare$edges
data_vertices <- flare$vertices

#KEGG
data_KEGG <- readr::read_csv("https://bizard-1301043367.cos.ap-guangzhou.myqcloud.com/data_KEGG.csv")

#KEGG_type
data_KEGG_type <- readr::read_csv("https://bizard-1301043367.cos.ap-guangzhou.myqcloud.com/data_KEGG_type.csv")
data_KEGG_type$pvalue_log <- -log10(data_KEGG_type$pvalue)
summary(data_KEGG_type)
data_KEGG_type1 <- data_KEGG_type %>%
  dplyr::select(type, subtype, PW, pvalue_log, NES) %>%
  arrange(type, subtype)

# Create visualization
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
# ... (see full tutorial for more)
```

## Key Parameters
- `group`: Maps `id` to the group aesthetic
- `fill`: Maps `NES` to the fill aesthetic
- `size`: Maps `pvalue_log` to the size aesthetic
- `alpha`: Controls transparency (0 = fully transparent, 1 = opaque)
- `width`: Controls element width
- `position`: Position adjustment (identity, dodge, stack, fill)
- `stat`: Statistical transformation to use
- `theme`: Plot theme; tutorial uses `theme_void()`

## Tips
- Customize color scales with `scale_fill_manual()` or `scale_color_brewer()`
- Adjust text size with `theme(text = element_text(size = 14))` for presentations
- Ensure proportions sum to 100% and consider using a colorblind-friendly palette

## Full Tutorial
https://openbiox.github.io/Bizard/Composition/CircularPacking.html
