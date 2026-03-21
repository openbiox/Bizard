# Skill: China Map (R)

## Category
Hiplot

## When to Use
::: callout-note
**Hiplot website**

## Required R Packages
- RColorBrewer
- data.table
- ggplot2
- jsonlite

## Minimal Reproducible Code
```r
# China Map
p <- ggplot(dt_map, aes(x = long, y = lat, group = group, fill = Value)) +
  labs(fill = "Value") +
  geom_polygon() +
  geom_path() +
  coord_fixed() +
  scale_fill_gradientn(
    colours = colorRampPalette(rev(brewer.pal(11,"RdYlBu")))(500),
    na.value = "grey10",
    limits = c(0, max(dt_map$Value) * 1.2)) +
    ggtitle("China Map Plot") +
  theme_minimal()

p
```

## Full Tutorial
https://openbiox.github.io/Bizard/Hiplot/100-map-china.html
