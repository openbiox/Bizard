# Skill: Point (SD) (R)

## Category
Hiplot

## When to use
::: callout-note
**Hiplot website**

## Required R packages
- data.table
- dplyr
- grafify
- jsonlite

## Minimal reproducible code
```r
# Point (SD)
p <- plot_point_sd(data = data, Student, Doubling_time, symsize = 5,
                   symthick = 0.5, s_alpha = 1, ewid = 0, symshape = 21,
                   all_alpha = 0) +
  geom_hline(aes(yintercept = median), colour = 'black', linetype = 2, 
             size = 0.5) +
  xlab(group) + ylab(y) + 
  guides(fill = guide_legend(title = group)) +
  ggtitle("Point-SD") +
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
https://openbiox.github.io/Bizard/Hiplot/142-point-sd.html
