# Skill: Circle Packing (R)

## Category
Hiplot

## When to Use
Circle packing is a visualization method used to display the differences in quantity among different categories.

## Required R Packages
- data.table
- ggplot2
- jsonlite
- packcircles
- viridis

## Minimal Reproducible Code
```r
# Load packages
library(data.table)
library(ggplot2)
library(jsonlite)
library(packcircles)
library(viridis)

# Prepare data
# Load data
data <- data.table::fread(jsonlite::read_json("https://hiplot.cn/ui/basic/circle-packing/data.json")$exampleData$textarea[[1]])
data <- as.data.frame(data)

# convert data structure
packing <- circleProgressiveLayout(data[["v"]], sizetype = "area")
data <- cbind(data, packing)
dat_gg <- circleLayoutVertices(packing, npoints = 50)
colors <- c("#E57164","#F8ECA7","#9389C1","#3F9C78","#769F8D","#E5F9A9","#7CE9A4",
            "#CE9FCA","#78F197","#8BB085","#D88880","#A6E4C3","#F7F6B1","#C5E69A",
            "#F45FDE","#5CF371","#9259CF","#2B6D9B","#F3C096","#EEADBE")
dat_gg$value <- rep(colors, each = 51)

# View data
head(data)

# Create visualization
# Circle Packing
p <- ggplot() +
  geom_polygon(data = dat_gg, aes(x, y, group = id, fill = value), colour = "black", alpha = 0.4) +
  scale_fill_manual(values = magma(nrow(data))) +
  theme_void() +
  theme(legend.position = "none") +
  coord_equal() +
  scale_size_continuous(range = c(2.3, 4.5)) +
  geom_text(data = data, aes(x, y, size = v, label = g), vjust = 0) +
  geom_text(data = data, aes(x, y, label = v, size = v), vjust = 1.2)

p
```

## Key Parameters
- `group`: Maps `id` to the group aesthetic
- `fill`: Maps `value` to the fill aesthetic
- `size`: Maps `v` to the size aesthetic
- `alpha`: Controls transparency (0 = fully transparent, 1 = opaque)
- `position`: Position adjustment (identity, dodge, stack, fill)
- `theme`: Plot theme; tutorial uses `theme_void()`

## Tips
- Customize color scales with `scale_fill_manual()` or `scale_color_brewer()`
- Adjust text size with `theme(text = element_text(size = 14))` for presentations
- See the full tutorial for additional customization options and advanced examples

## Full Tutorial
https://openbiox.github.io/Bizard/Hiplot/021-circle-packing.html
