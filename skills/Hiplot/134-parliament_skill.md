# Skill: Parliament (R)

## Category
Hiplot

## When to Use
::: callout-note
**Hiplot website**

## Required R Packages
- data.table
- ggplot2
- ggpol
- jsonlite

## Minimal Reproducible Code
```r
# Parliament
p <- ggplot(data) +
  geom_parliament(alpha = 1, aes(seats = value, fill = group), color = "black") +
  coord_fixed() +
  scale_fill_discrete(name = "group", labels = unique(data$group)) +
  scale_fill_manual(values = c("#E64B35FF","#4DBBD5FF","#00A087FF","#3C5488FF",
                                "#F39B7FFF")) +
  ggtitle("Parliament Plot") +
  theme_void() +
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = 0.5))

p
```

## Full Tutorial
https://openbiox.github.io/Bizard/Hiplot/134-parliament.html
