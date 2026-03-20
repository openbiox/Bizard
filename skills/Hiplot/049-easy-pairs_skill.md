# Skill: Easy Pairs (R)

## Category
Hiplot

## When to use
::: callout-note
**Hiplot website**

## Required R packages
- GGally
- data.table
- jsonlite

## Minimal reproducible code
```r
# Easy Pairs
p <- ggpairs(data, columns = c("total_bill", "time", "tip"),
             mapping = aes_string(color = "gender")) +
  ggtitle("Easy Pairs") +
  scale_fill_manual(values = c("#3B4992FF","#EE0000FF")) +
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
https://openbiox.github.io/Bizard/Hiplot/049-easy-pairs.html
