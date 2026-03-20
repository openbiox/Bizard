# Skill: World Map (R)

## Category
Hiplot

## When to use
::: callout-note
**Hiplot website**

## Required R packages
- RColorBrewer
- data.table
- ggplot2
- jsonlite

## Minimal reproducible code
```r
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

## Full tutorial
https://openbiox.github.io/Bizard/Hiplot/116-map-world.html
