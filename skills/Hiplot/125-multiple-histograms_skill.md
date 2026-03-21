# Skill: Multiple Histograms (R)

## Category
Hiplot

## When to Use
::: callout-note
**Hiplot website**

## Required R Packages
- data.table
- ggplot2
- jsonlite

## Minimal Reproducible Code
```r
# Multiple Histograms
p <- ggplot(data, aes(x = value, fill = type)) +
  geom_histogram(color = "black", alpha = 0.5, 
                 position = "identity", binwidth = 0.3) +
  scale_fill_manual(values = c("#BC3C29FF","#0072B5FF")) +
  theme_bw()

p
```

## Full Tutorial
https://openbiox.github.io/Bizard/Hiplot/125-multiple-histograms.html
