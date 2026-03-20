# Skill: Sankey (R)

## Category
Hiplot

## When to use
::: callout-note
**Hiplot website**

## Required R packages
- data.table
- ggalluvial
- ggplot2
- jsonlite

## Minimal reproducible code
```r
# Sankey
p <- ggplot(data_rename, aes(y = value, axis1 = axis1, axis2 = axis2)) +
  geom_alluvium(alpha = 1, aes(fill = data1[, colnames(data1) == "Sex"]),
                width = 0, reverse = FALSE) +
  scale_x_discrete(limits = usr_axis, expand = c(0.02, 0.1)) +
  ylab("") +
  scale_fill_discrete(name = "Sex") +
  coord_flip() +
  geom_stratum(alpha = 1, width = 1 / 8, reverse = FALSE, fill = band_color,
               color = "white") +
  geom_text(stat = "stratum", infer.label = TRUE, reverse = FALSE) +
  ggtitle("Sankey plot") +
  guides(fill = guide_legend(title = "Sex")) +
  scale_fill_manual(values = c("#00468BFF", "#ED0000FF")) +
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
https://openbiox.github.io/Bizard/Hiplot/158-sankey.html
