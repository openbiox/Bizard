# Skill: Bubble (R)

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
# Bubble
p <- ggplot(data, aes(Ratio, Term)) +
  geom_point(aes(size = Count, colour = -log10(PValue))) +
  scale_colour_gradient(low = "#00438E", high = "#E43535") +
  labs(colour = "-log10 (PValue)", size = "Count", x = "Ratio", y = "Term", 
       title = "Bubble Plot") +
  scale_x_continuous(limits = c(0, max(data$Ratio) * 1.2)) +
  guides(color = guide_colorbar(order = 1), size = guide_legend(order = 2)) +
  scale_y_discrete(labels = function(x) {str_wrap(x, width = 65)}) +
  theme_bw() +
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
https://openbiox.github.io/Bizard/Hiplot/016-bubble.html
