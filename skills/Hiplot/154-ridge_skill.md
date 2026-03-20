# Skill: Ridge (R)

## Category
Hiplot

## When to use
::: callout-note
**Hiplot website**

## Required R packages
- data.table
- ggplot2
- ggridges
- ggthemes
- jsonlite

## Minimal reproducible code
```r
# Ridge
p <- ggplot(data, aes(x = value, y = group, fill = group, col = group)) +
  geom_density_ridges(scale = 5, alpha = 0.8) +
  labs(x = "value", y = "group") +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "none") +
  ggtitle("Ridge Plot") +
  guides(color = guide_legend(reverse = TRUE),
         fill = guide_legend(reverse = TRUE)) +
  scale_fill_manual(values = c("#e04d39","#5bbad6","#1e9f86")) +
  scale_color_manual(values = c("#e04d39","#5bbad6","#1e9f86")) +
  theme_stata() +
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

## Full tutorial
https://openbiox.github.io/Bizard/Hiplot/154-ridge.html
