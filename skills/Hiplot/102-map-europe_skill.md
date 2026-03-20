# Skill: Europe Map (R)

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
# Europe Map
p <- ggplot(dt_map) +
  geom_polygon(aes(x = long, y = lat, group = group, fill = Value),
               alpha = 0.9, size = 0.5) +
  geom_path(aes(x = long, y = lat, group = group), color = "black", size = 0.2) +
  coord_fixed() +
  scale_fill_gradientn(
    colours = colorRampPalette(rev(brewer.pal(11,"RdYlBu")))(500),
    breaks = seq(min(data$value), max(data$value), 
                 round((max(data$value)-min(data$value))/7)),
    name = "Color Key",
    guide = guide_legend(
      direction = "vertical", keyheight = unit(1, units = "mm"),
      keywidth = unit(8, units = "mm"),
      title.position = "top", title.hjust = 0.5, label.hjust = 0.5,
      nrow = 1, byrow = T, reverse = F, label.position = "bottom")) +
  theme(text = element_text(color = "#3A3F4A"),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "top",
        legend.text = element_text(size = 4 * 1.5, color = "black"),
        legend.title = element_text(size = 5 * 1.5, color = "black"),
        plot.title = element_text(
          face = "bold", size = 5 * 1.5, hjust = 0.5, 
          margin = margin(t = 4, b = 5), color = "black"),
        plot.background = element_rect(fill = "#FFFFFF", color = "#FFFFFF"),
        panel.background = element_rect(fill = "#FFFFFF", color = NA),
        legend.background = element_rect(fill = "#FFFFFF", color = NA),
        plot.margin = unit(c(1.5, 1.5, 1.5, 1.5), "cm")) +
  labs(x = NULL, y = NULL, title = "Europe Map")

p
```

## Full tutorial
https://openbiox.github.io/Bizard/Hiplot/102-map-europe.html
