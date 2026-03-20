# Skill: Motif Plot (R)

## Category
Omics

## When to use
For visualizing motif logos, ggseqlogo is an R package based on ggplot2 specifically designed for plotting logos from sequence motifs. Compared to other motif visualization tools, ggseqlogo boasts advantages such as concise syntax, flexible output formats, and full compatibility with the ggplot2 ecosystem. The package supports various sequence input formats, including position-frequency matrices (PFM), position-weight matrices (PWM), and sequence vectors, and provides rich customization optio...

## Required R packages
- cowplot
- ggplot2
- ggseqlogo
- gridExtra

## Minimal reproducible code
```r
# Using sequence vectors
ggseqlogo(seqs_dna$MA0001.1)
# Using PFM matrix
ggseqlogo(pfms_dna$MA0018.2)
# Plotting using ggplot syntax
ggplot() + geom_logo( seqs_dna$MA0001.1 ) + theme_logo()
```

## Full tutorial
https://openbiox.github.io/Bizard/Omics/MotifPlot.html
