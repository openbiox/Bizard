# Skill: Beeswarm (R)

## Category
Hiplot

## When to use
::: callout-note
**Hiplot website**

## Required R packages
- data.table
- ggbeeswarm
- ggthemes
- jsonlite

## Minimal reproducible code
```r
# Beeswarm
p <- ggplot(data, aes(Group, y, color = Group)) +
  geom_beeswarm(alpha = 1, size = 0.8) +
  labs(x = NULL, y = "value") +
  ggtitle("BeeSwarm Plot") +
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
https://openbiox.github.io/Bizard/Hiplot/012-beeswarm.html
