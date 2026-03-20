# Skill: Dual Y Axis Chart (R)

## Category
Hiplot

## When to use
::: callout-note
**Hiplot website**

## Required R packages
- data.table
- ggplot2
- jsonlite

## Minimal reproducible code
```r
# Dual Y Axis Chart
p <- ggplot(data, aes(x = x)) +
  geom_line(aes(y = data[, 2]), size = 1, color = "#D72C15") +
  geom_line(aes(y = data[, 3] / as.numeric(10)), size = 1, color = "#02657B") +
  scale_y_continuous(
    name = colnames(data)[2],
    sec.axis = sec_axis(~ . * as.numeric(10), name = colnames(data)[3])) +
  ggtitle("Dual Y Axis Chart") + xlab("x") +
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
https://openbiox.github.io/Bizard/Hiplot/047-dual-y-axis.html
