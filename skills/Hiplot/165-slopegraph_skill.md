# Skill: Slopegraph (R)

## Category
Hiplot

## When to use
::: callout-note
**Hiplot website**

## Required R packages
- CGPfunctions
- data.table
- ggplot2
- jsonlite

## Minimal reproducible code
```r
# Slopegraph
p <- newggslopegraph(data, year, lifeExp, country) +
  labs(subtitle = "", title = "Slope Graph", x = "Life Expectancy (years)",
       y = "country", caption = "") +
  scale_color_manual(values = c("#3B4992FF", "#EE0000FF", "#008B45FF",
                               "#631879FF", "#008280FF", "#BB0021FF")) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

p
```

## Full tutorial
https://openbiox.github.io/Bizard/Hiplot/165-slopegraph.html
