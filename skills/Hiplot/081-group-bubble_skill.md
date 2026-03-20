# Skill: Group Bubble (R)

## Category
Hiplot

## When to use
::: callout-note
**Hiplot website**

## Required R packages
- data.table
- ggplot2
- jsonlite

## Minimal reproducible code
```r
# Group Bubble
p <- ggplot(data = data, aes(x = Sepal.Length, y = Sepal.Width, 
                             size = Petal.Width, color = Species)) +
  geom_point(alpha = 0.7) +
  scale_size(range = c(1, 4)) +
  scale_color_manual(values = c("#e04d39","#5bbad6","#1e9f86")) +
  theme_bw()

p
```

## Full tutorial
https://openbiox.github.io/Bizard/Hiplot/081-group-bubble.html
