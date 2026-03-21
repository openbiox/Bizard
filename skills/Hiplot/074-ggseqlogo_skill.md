# Skill: Seqlogo (R)

## Category
Hiplot

## When to Use
::: callout-note
**Hiplot website**

## Required R Packages
- data.table
- ggplot2
- ggseqlogo
- jsonlite

## Minimal Reproducible Code
```r
# Seqlogo
p <- ggseqlogo(
  data,
  ncol = 4,
  col_scheme = "nucleotide",
  seq_type = "dna",
  method = "bits") + 
  theme(plot.title = element_text(hjust = 0.5))

p
```

## Full Tutorial
https://openbiox.github.io/Bizard/Hiplot/074-ggseqlogo.html
