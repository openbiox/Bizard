# Skill: Waterfalls (R)

## Category
Hiplot

## When to use
::: callout-note
**Hiplot website**

## Required R packages
- data.table
- ggplot2
- jsonlite
- waterfalls

## Minimal reproducible code
```r
# Waterfalls
p <- waterfall(data, rect_text_labels = data$value, rect_text_size = 1,
    rect_text_labels_anchor = "centre", calc_total = T,
    total_axis_text = "Total", total_rect_text = sum(data$value),
    total_rect_color = "steelblue", total_rect_text_color = "black",
    rect_width = 0.7, rect_border = "black", draw_lines = TRUE,
    linetype = 2, fill_by_sign = F, 
    fill_colours = c("#E64B35FF","#4DBBD5FF","#00A087FF","#3C5488FF","#F39B7FFF",
                     "#8491B4FF"),
    scale_y_to_waterfall = T) +
  theme_bw() +
  theme(axis.text = element_text(size = 12),
        plot.title = element_text(hjust = 0.5)) +
  labs(title = "Waterfalls Plot")

p
```

## Full tutorial
https://openbiox.github.io/Bizard/Hiplot/186-waterfalls.html
