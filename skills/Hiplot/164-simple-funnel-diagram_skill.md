# Skill: Simple Funnel Diagram (R)

## Category
Hiplot

## When to Use
::: callout-note
**Hiplot website**

## Required R Packages
- data.table
- echarts4r
- jsonlite
- magrittr

## Minimal Reproducible Code
```r
# Simple Funnel Diagram
p <- data %>%
  e_charts() %>%
  e_funnel(value, key) %>%
  e_title("Funnel") %>%
  e_theme("macarons")

p
```

## Full Tutorial
https://openbiox.github.io/Bizard/Hiplot/164-simple-funnel-diagram.html
