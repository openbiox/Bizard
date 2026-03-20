# Skill: Dotchart (R)

## Category
Hiplot

## When to use
::: callout-note
**Hiplot website**

## Required R packages
- data.table
- ggpubr
- jsonlite

## Minimal reproducible code
```r
# Dotchart
p <- ggdotchart(data, x = "Name", y = "Value", group = "Group", color = "Group",
                rotate = T, sorting = "descending",
                y.text.col = F, add = "segments", dot.size = 2) +
  xlab("Name") +
  ylab("Value") +
  ggtitle("DotChart Plot") +
  scale_color_manual(values = c("#e04d39","#5bbad6","#1e9f86")) +
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
https://openbiox.github.io/Bizard/Hiplot/046-dotchart.html
