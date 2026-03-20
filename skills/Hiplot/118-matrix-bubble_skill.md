# Skill: Matrix Bubble (R)

## Category
Hiplot

## When to use
::: callout-note
**Hiplot website**

## Required R packages
- data.table
- ggalluvial
- ggplot2
- jsonlite

## Minimal reproducible code
```r
# Matrix Bubble
p <- ggplot(data = data, aes(x = x, y = y, size = value, color = y)) +
  geom_point(alpha = 1) +
  labs(title = "Matrix Bubble") +
  guides(color = FALSE) +
  theme(panel.background = element_blank(),
        panel.grid.major = element_line(colour = "gray"),
        strip.background = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA),
        panel.spacing = unit(0, "lines"),
        plot.title = element_text(size = 12, hjust = 0.5),
        text = element_text(family = "Arial"),
        legend.title = element_text(size = 10),
        axis.text.x = element_text(angle=0, hjust=0.5, vjust=1)) +
  facet_grid(~group, scales = 'fixed', margins = F) +
  scale_color_manual(values = c(
    "#3B4992FF","#EE0000FF","#008B45FF","#631879FF","#008280FF","#BB0021FF",
    "#5F559BFF","#A20056FF","#808180FF","#1B1919FF"))

p
```

## Full tutorial
https://openbiox.github.io/Bizard/Hiplot/118-matrix-bubble.html
