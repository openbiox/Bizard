# Skill: Density (R)

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
# Density
data["group_add_by_code"] <- "g1"

p <- ggplot(data, aes_(as.name(colnames(data[1])))) +
  geom_density(col = "white", alpha = 0.85,
               aes_(fill = as.name(colnames(data[2])))) +
  ggtitle("") +
  scale_fill_manual(values = c("#e04d39","#5bbad6")) +
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
https://openbiox.github.io/Bizard/Hiplot/040-density.html
