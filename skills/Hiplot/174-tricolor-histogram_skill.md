# Skill: Tricolor Histogram (R)

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
# Tricolor Histogram
p <- ggplot(data, aes(x = values, fill = draw_color)) +
  geom_histogram(alpha = 0.5, binwidth = 0.05, position = "identity") +
    scale_fill_manual(values = c("#3F51B5", "#006064", "#F44336")) +
  theme_bw()

p
```

## Full tutorial
https://openbiox.github.io/Bizard/Hiplot/174-tricolor-histogram.html
