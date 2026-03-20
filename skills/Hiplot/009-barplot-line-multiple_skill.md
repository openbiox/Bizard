# Skill: Multiple Barplot&Line (R)

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
- reshape2

## Minimal reproducible code
```r
# Multiple Line
p <- ggplot(data = data_melt, aes(x = age, y = value, group = variable,
                                  colour = variable)) +
  geom_line(alpha = 1, size = 1) +
  geom_point(aes(shape = variable), alpha = 1, size = 3) +
  labs(title = "Line (Multiple)", x = "X Lable", y = "Value") +
  scale_color_manual(values = c("#3B4992FF","#EE0000FF","#008B45FF","#631879FF",
                               "#008280FF","#BB0021FF")) +
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
https://openbiox.github.io/Bizard/Hiplot/009-barplot-line-multiple.html
