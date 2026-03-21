# Skill: Pyramid Chart (R)

## Category
Hiplot

## When to Use
::: callout-note
**Hiplot website**

## Required R Packages
- data.table
- ggcharts
- jsonlite

## Minimal Reproducible Code
```r
# Pyramid Chart
p <- pyramid_chart(data = data, x = age, y = pop, group = sex, 
                   title = "", sort = "no", bar_colors = c("#C20B01","#196ABD")) +
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
https://openbiox.github.io/Bizard/Hiplot/144-pyramid-chart.html
