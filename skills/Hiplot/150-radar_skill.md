# Skill: Radar (R)

## Category
Hiplot

## When to use
::: callout-note
**Hiplot website**

## Required R packages
- data.table
- dplyr
- ggplot2
- ggradar
- jsonlite
- scales
- tibble

## Minimal reproducible code
```r
# Radar
p <- ggradar(data_radar, gridline.max.linetype = 1, group.point.size = 4,
             group.line.width = 1, font.radar = "Arial", fill.alpha = 0.5,
             gridline.min.colour = "grey", gridline.mid.colour = "#007A87",
             gridline.max.colour = "grey") +
  ggtitle("Radar Plot") +
  scale_color_manual(values = c("#E64B35FF","#4DBBD5FF","#00A087FF","#3C5488FF")) +
  theme(text = element_text(family = "Arial"),
        plot.title = element_text(size = 12,hjust = 0.5),
        axis.title = element_blank(),
        axis.text = element_text(size = 10),
        axis.text.x = element_blank(),
        axis.title.y=element_blank(),
        axis.ticks.y=element_blank(),
        legend.position = "right",
        legend.direction = "vertical",
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 10))
 
p
```

## Full tutorial
https://openbiox.github.io/Bizard/Hiplot/150-radar.html
