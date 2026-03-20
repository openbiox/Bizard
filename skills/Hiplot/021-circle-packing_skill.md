# Skill: Circle Packing (R)

## Category
Hiplot

## When to use
::: callout-note
**Hiplot website**

## Required R packages
- data.table
- ggplot2
- jsonlite
- packcircles
- viridis

## Minimal reproducible code
```r
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

## Full tutorial
https://openbiox.github.io/Bizard/Hiplot/021-circle-packing.html
