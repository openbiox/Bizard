# Skill: Barplot Gradient (R)

## Category
Hiplot

## When to use
::: callout-note
**Hiplot website**

## Required R packages
- data.table
- ggplot2
- jsonlite
- stringr

## Minimal reproducible code
```r
# Barplot Gradient
p <- ggplot(data, aes(x = Term, y = Count, fill = -log10(PValue))) +
  geom_bar(stat = "identity") +
  ggtitle("GO BarPlot") +
  scale_fill_continuous(low = "#00438E", high = "#E43535") +
  scale_x_discrete(labels = function(x) {str_wrap(x, width = 65)}) +
  labs(fill = "-log10 (PValue)", y = "Term", x = "Count") +
  coord_flip() +
  theme_bw() +
  theme(text = element_text(family = "Arial"),
        plot.title = element_text(size = 12,hjust = 0.5),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 10),
        axis.text.x = element_text(angle = 0, hjust = 0.5),
        legend.position = "right",
        legend.direction = "vertical",
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 10))

p
```

## Full tutorial
https://openbiox.github.io/Bizard/Hiplot/008-barplot-gradient.html
