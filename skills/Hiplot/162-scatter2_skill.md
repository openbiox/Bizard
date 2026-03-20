# Skill: Scatter2 (R)

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
# scatter2
symsize <- data[,"gear"]
data[,"gear"] <- factor(data[,"gear"], levels = unique(data[,"gear"]))
p <- ggplot(data, aes(x = mpg, y = disp)) + 
  geom_point(alpha = 1, aes(size = gear, fill = gear), shape = 21, stroke = 0.5) +
  labs(fill = "gear", color = "gear") +
  guides(x = guide_axis(angle = 0),
         fill = guide_legend(title = "gear"),
         color = FALSE,
         size = guide_legend(title = "gear")) +
  ggtitle("Scatter2 Plot") +
  scale_fill_grafify() +
  theme_classic(base_size = 20) +
  theme(text = element_text(family = "Arial"),
        strip.background = element_blank(),
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
https://openbiox.github.io/Bizard/Hiplot/162-scatter2.html
