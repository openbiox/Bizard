# Skill: Pyramid Chart 2 (R)

## Category
Hiplot

## When to use
::: callout-note
**Hiplot website**

## Required R packages
- apyramid
- data.table
- ggplot2
- jsonlite

## Minimal reproducible code
```r
# Pyramid Chart 2
p <- age_pyramid(data, "age_group", split_by = "Gender") + 
  scale_fill_manual(values = c("#BC3C29FF","#0072B5FF")) +
  xlab("Age group") + 
  ylab("Gender") +
  theme_classic() +
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
https://openbiox.github.io/Bizard/Hiplot/145-pyramid-chart2.html
