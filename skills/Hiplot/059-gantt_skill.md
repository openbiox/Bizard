# Skill: Gantt (R)

## Category
Hiplot

## When to use
::: callout-note
**Hiplot website**

## Required R packages
- data.table
- ggthemes
- jsonlite
- tidyverse

## Minimal reproducible code
```r
# Gantt
p <- ggplot(data_gather, aes(date, sample, color = item)) +
  geom_line(size = 10, alpha = 1) +
  labs(x = "Time", y = "sample", title = "Gantt Plot") +
  theme(axis.ticks = element_blank()) +
  scale_color_manual(values = c("#e04d39","#5bbad6","#1e9f86")) +
  theme_stata() +
  theme(text = element_text(family = "Arial"),
        plot.title = element_text(size = 12,hjust = 0.5),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 10),
        axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 1),
        legend.position = "right",
        legend.direction = "vertical",
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 10))

p
```

## Full tutorial
https://openbiox.github.io/Bizard/Hiplot/059-gantt.html
