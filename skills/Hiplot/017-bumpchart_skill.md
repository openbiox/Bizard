# Skill: Bumpchart (R)

## Category
Hiplot

## When to use
::: callout-note
**Hiplot website**

## Required R packages
- data.table
- dplyr
- ggbump
- ggplot2
- jsonlite

## Minimal reproducible code
```r
# Bumpchart
p <- ggplot(data, aes(x = x, y = y, color = group)) +
  geom_bump(size = 1.5) +
  geom_point(size = 5) +
  geom_text(data = data %>% filter(x == min(x)),
            aes(x = x - 0.1, label = group),
            size = 5, hjust = 1) +
  geom_text(data = data %>% filter(x == max(x)),
            aes(x = x + 0.1, label = group),
            size = 5, hjust = 0) +
  theme_void() +
  theme(legend.position = "none") +
  scale_color_manual(values = c("#0571B0","#92C5DE","#F4A582","#CA0020"))

p
```

## Full tutorial
https://openbiox.github.io/Bizard/Hiplot/017-bumpchart.html
