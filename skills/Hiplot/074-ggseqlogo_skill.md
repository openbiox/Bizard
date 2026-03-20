# Skill: Seqlogo (R)

## Category
Hiplot

## When to use
::: callout-note
**Hiplot website**

## Required R packages
- data.table
- ggplot2
- ggseqlogo
- jsonlite

## Minimal reproducible code
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

## Full tutorial
https://openbiox.github.io/Bizard/Hiplot/074-ggseqlogo.html
