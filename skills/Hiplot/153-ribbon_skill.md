# Skill: Ribbon (R)

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
# Ribbon
p <- ggplot(data, aes(xvalue, yvalue, fill = group)) +
  geom_ribbon(alpha = 0.2, aes(ymin = yvalue1, ymax = yvalue2)) +
  geom_line(aes(y = yvalue, color = group), lwd = 1) +
  geom_line(aes(y = yvalue1, color = group), linetype = "dotted") +
  geom_line(aes(y = yvalue2, color = group), linetype = "dotted") +
  ylab("y axis value") +
  xlab("x axis value") +
  ggtitle("Ribbon Plot") +
  scale_fill_manual(values = c("#e04d39","#5bbad6")) +
  scale_color_manual(values = c("#e04d39","#5bbad6")) +
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
https://openbiox.github.io/Bizard/Hiplot/153-ribbon.html
