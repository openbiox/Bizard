# Skill: Diverging Scale (R)

## Category
Hiplot

## When to use
::: callout-note
**Hiplot website**

## Required R packages
- data.table
- ggcharts
- jsonlite

## Minimal reproducible code
```r
# Diverging Scale Barplot
fill_colors <- c("#C20B01", "#196ABD")
fill_colors <- fill_colors[c(any(data[, "y"] > 0), any(data[, "y"] < 0))]
p <- diverging_bar_chart(data = data, x = x, y = y, bar_colors = fill_colors,
                         text_color = '#000000') + 
  theme(axis.text.x = element_text(color = "#000000"),
        axis.title.x = element_text(colour = "#000000"),
        axis.title.y = element_text(colour = "#000000"),
        plot.background = element_blank()) + 
  labs(x = "model", y = "scale(hp)", title = "")

p
```

## Full tutorial
https://openbiox.github.io/Bizard/Hiplot/043-diverging-scale.html
