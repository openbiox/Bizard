# Skill: World Map (R)

## Category
Hiplot

## When to Use
Create a World Map using R with the Hiplot platform's approach. Suitable for biomedical data visualization with publication-quality output.

## Required R Packages
- RColorBrewer
- data.table
- ggplot2
- jsonlite

## Minimal Reproducible Code
```r
# Load packages
library(RColorBrewer)
library(data.table)
library(ggplot2)
library(jsonlite)

# Prepare data
# Load data
data <- data.table::fread(jsonlite::read_json("https://hiplot.cn/ui/basic/map-world/data.json")$exampleData$textarea[[1]])
data <- as.data.frame(data)
dt_map <- readRDS(url("https://download.hiplot.cn/ui/basic/map-world/world.rds"))

# Convert data structure
dt_map$Value <- data$death_rate[match(dt_map$ENG_NAME, data$region)]

# View data
head(data)

# Create visualization
# World Map
p <- ggplot(dt_map) +
  geom_polygon(aes(x = long, y = lat, group = group, fill = Value),
               alpha = 0.9, size = 0.5) +
  geom_path(aes(x = long, y = lat, group = group), 
            color = "black", size = 0.2) +
  coord_fixed() +
  scale_fill_gradientn(
    colours = colorRampPalette(rev(brewer.pal(11,"RdYlBu")))(500),
    na.value = "grey10",
    limits = c(0, max(dt_map$Value) * 1.2)) +
    ggtitle("World Map Plot") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "bottom", legend.direction = "horizontal")

p
```

## Key Parameters
- `x`: Maps `long` to the x aesthetic
- `y`: Maps `lat` to the y aesthetic
- `group`: Maps `group` to the group aesthetic
- `fill`: Maps `Value` to the fill aesthetic
- `alpha`: Controls transparency (0 = fully transparent, 1 = opaque)
- `position`: Position adjustment (identity, dodge, stack, fill)
- `theme`: Plot theme; tutorial uses `theme_minimal()`

## Tips
- Use `theme_minimal()` or `theme_bw()` for clean, publication-ready plots
- Customize color scales with `scale_fill_manual()` or `scale_color_brewer()`
- See the full tutorial for additional customization options and advanced examples

## Full Tutorial
https://openbiox.github.io/Bizard/Hiplot/116-map-world.html
