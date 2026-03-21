# Skill: D3 Wordcloud (R)

## Category
Hiplot

## When to Use
::: callout-note
**Hiplot website**

## Required R Packages
- d3wordcloud
- data.table
- jsonlite

## Minimal Reproducible Code
```r
# D3 Wordcloud
p <- d3wordcloud(
  words = data[, 1], 
  freqs = data[, 2],
  padding = 0,
  rotate.min = 0,
  rotate.max = 0,
  size.scale = "linear",
  color.scale = "linear",
  spiral = "archimedean",
  font = "Arial",
  rangesizefont = c(10, 90)
)

p
```

## Full Tutorial
https://openbiox.github.io/Bizard/Hiplot/036-d3-wordcloud.html
