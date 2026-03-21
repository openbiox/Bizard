# Skill: Contour (XY) (R)

## Category
Hiplot

## When to Use
Contour plot (XY) is a data processing method that reflects data density through contour line.

## Required R Packages
- data.table
- ggisoband
- ggplot2
- jsonlite

## Minimal Reproducible Code
```r
# Load packages
library(data.table)
library(ggisoband)
library(ggplot2)
library(jsonlite)

# Prepare data
# Load data
data <- data.table::fread(jsonlite::read_json("https://hiplot.cn/ui/basic/contour-xy/data.json")$exampleData$textarea[[1]])
data <- as.data.frame(data)

# convert data structure
colnames(data) <- c("xvalue", "yvalue")

# View data
head(data)

# Create visualization
# Contour (XY)
p <- ggplot(data, aes(xvalue, yvalue)) +
  geom_density_bands(
    alpha = 1,
    aes(fill = stat(density)), color = "gray40", size = 0.2
    ) +
  geom_point(alpha = 1, shape = 21, fill = "white") +
  scale_fill_viridis_c(guide = "legend") +
  ylab("value2") +
  xlab("value1") +
  ggtitle("Contour-XY Plot") +
  theme_bw() +
  theme(text = element_text(family = "Arial"),
        plot.title = element_text(size = 12,hjust = 0.5),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 10),
        axis.text.x = element_text(angle = 0, hjust = 0.5,vjust = 1),
        legend.position = "right",
        legend.direction = "vertical",
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 10))

p
```

## Key Parameters
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
https://openbiox.github.io/Bizard/Hiplot/028-contour-xy.html
