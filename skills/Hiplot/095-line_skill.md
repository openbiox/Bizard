# Skill: Line (R)

## Category
Hiplot

## When to Use
::: callout-note
**Hiplot website**

## Required R Packages
- data.table
- ggplot2
- ggthemes
- jsonlite

## Minimal Reproducible Code
```r
# Line
p <- ggplot(data, aes(x = Value1, y = Value2)) +
  geom_line(alpha = 1, aes(color = Group, linetype = Group)) +
  geom_point(aes(color = Group, shape = Group)) +
  ggtitle("Line Regression Plot") +
  scale_fill_manual(values = c("#e04d39","#5bbad6")) +
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

## Full Tutorial
https://openbiox.github.io/Bizard/Hiplot/095-line.html
