# Skill: Gradient Scatter (R)

## Category
Hiplot

## When to use
::: callout-note
**Hiplot website**

## Required R packages
- data.table
- ggplot2
- grafify
- jsonlite

## Minimal reproducible code
```r
# Gradient Scatter
p <- ggplot(data, aes(x = mpg, y = disp)) + 
  geom_point(aes(fill = gear), size = 5, alpha = 1, shape = 21, stroke = 0.5) +
  labs(fill = "gear", color = "gear") +
  theme_classic(base_size = 10) +
  theme(strip.background = element_blank()) +
  guides(x = guide_axis(angle = 0)) +
  scale_fill_gradient(low = "#00438E", high = "#E43535") +
  scale_color_gradient(low = "#00438E", high = "#E43535") + 
  guides(fill = guide_legend(title = "gear"),
         size = guide_legend(title = "gear")) +
  ggtitle("Scatter-gradient Plot") +
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
https://openbiox.github.io/Bizard/Hiplot/160-scatter-gradient.html
