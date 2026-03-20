# Skill: Multiple Histograms (R)

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
# Multiple Histograms
p <- ggplot(data, aes(x = value, fill = type)) +
  geom_histogram(color = "black", alpha = 0.5, 
                 position = "identity", binwidth = 0.3) +
  scale_fill_manual(values = c("#BC3C29FF","#0072B5FF")) +
  theme_bw()

p
```

## Full tutorial
https://openbiox.github.io/Bizard/Hiplot/125-multiple-histograms.html
