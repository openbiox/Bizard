# Skill: Contour (Matrix) (R)

## Category
Hiplot

## When to Use
The contour map (matrix) is a graph that displays three-dimensional data in a two-dimensional form

## Required R Packages
- cowplot
- data.table
- ggisoband
- ggplot2
- jsonlite
- reshape2

## Minimal Reproducible Code
```r
# Load packages
library(cowplot)
library(data.table)
library(ggisoband)
library(ggplot2)
library(jsonlite)
library(reshape2)

# Prepare data
# Load data
data <- data.table::fread(jsonlite::read_json("https://hiplot.cn/ui/basic/contour-matrix/data.json")$exampleData$textarea[[1]])
data <- as.data.frame(data)

# convert data structure
data <- as.matrix(data)
colnames(data) <- NULL
data3d <- reshape2::melt(data)
names(data3d) <- c("x", "y", "z")

# View data
head(data3d)

# Create visualization
# Contour (Matrix)
complex_general_theme <- 
  theme(text = element_text(family = "Arial"),
        plot.title = element_text(size = 12,hjust = 0.5),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 10),
        axis.text.x = element_text(angle = 0, hjust = 0.5,vjust = 1),
        legend.position = "right",
        legend.direction = "vertical",
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 10))

p1 <- ggplot(data3d, aes(x, y, z = z)) +
  geom_isobands(
    alpha = 1,
    aes(color = stat(zmin)), fill = NA
  ) +
  scale_color_viridis_c() +
  coord_cartesian(expand = FALSE) +
  theme_bw() +
  complex_general_theme

p2 <- ggplot(data3d, aes(x, y, z = z)) +
  geom_isobands(
    alpha = 1,
    aes(fill = stat(zmin)), color = NA
  ) +
# ... (see full tutorial for more)
```

## Key Parameters
- `color`: Maps `stat` to the color aesthetic
- `fill`: Maps `stat` to the fill aesthetic
- `alpha`: Controls transparency (0 = fully transparent, 1 = opaque)
- `position`: Position adjustment (identity, dodge, stack, fill)
- `stat`: Statistical transformation to use
- `theme`: Plot theme; tutorial uses `theme_bw()`

## Tips
- Use `theme_minimal()` or `theme_bw()` for clean, publication-ready plots
- Customize color scales with `scale_fill_manual()` or `scale_color_brewer()`
- See the full tutorial for additional customization options and advanced examples

## Full Tutorial
https://openbiox.github.io/Bizard/Hiplot/027-contour-matrix.html
