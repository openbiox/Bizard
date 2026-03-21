# Skill: Tricolor Histogram (R)

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
# Tricolor Histogram
p <- ggplot(data, aes(x = values, fill = draw_color)) +
  geom_histogram(alpha = 0.5, binwidth = 0.05, position = "identity") +
    scale_fill_manual(values = c("#3F51B5", "#006064", "#F44336")) +
  theme_bw()

p
```

## Full Tutorial
https://openbiox.github.io/Bizard/Hiplot/174-tricolor-histogram.html
