# Skill: Chi-square-fisher Test (R)

## Category
Hiplot

## When to Use
::: callout-note
**Hiplot website**

## Required R Packages
- aplot
- data.table
- ggplot2
- jsonlite
- visdat

## Minimal Reproducible Code
```r
# Chi-square-fisher Test
p1 <- vis_value(final["statistic"]) + 
  scale_fill_gradientn(colours = c("#3362ab","#87b7d7","#e8e0db","#eea07d","#ad1c2e"))

p2 <- vis_expect(final["pvalue"], ~.x < 0.05) +
  scale_fill_manual(values = c("#1c438a","#e7120c"))

p <- p1+p2
p
```

## Full Tutorial
https://openbiox.github.io/Bizard/Hiplot/019-chi-square-fisher.html
