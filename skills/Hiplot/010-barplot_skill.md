# Skill: Barplot (R)

## Category
Hiplot

## When to use
::: callout-note
**Hiplot website**

## Required R packages
- data.table
- ggplot2
- ggthemes
- jsonlite

## Minimal reproducible code
```r
# Barplot
p <- ggplot(data, aes(x = dose, y = value, fill = treat)) +
  geom_bar(position = position_dodge(0.9), stat = "identity") +
  ggtitle("Bar Plot") +
  geom_text(aes(label = value), position = position_dodge(0.9), vjust = 1.5, color = "white", size = 3.5) +
  scale_fill_manual(values = c("#e04d39","#5bbad6","#1e9f86","#3c5488ff")) +
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
https://openbiox.github.io/Bizard/Hiplot/010-barplot.html
