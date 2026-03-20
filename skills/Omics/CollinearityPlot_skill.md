# Skill: Collinearity Plot (R)

## Category
Omics

## When to use
Collinearity plot is often used to compare genome sequences of different species, identify conserved homologous gene blocks and their arrangement order, and reveal changes in chromosome structure during evolution. This plot is widely used in the study of genome evolution, functional gene localization, and species relationship analysis.

## Required R packages
- RIdeogram

## Minimal reproducible code
```r
# Basic Collinearity Plot
ideogram(karyotype = karyotype_ternary_comparison, synteny = synteny_ternary_comparison)
convertSVG("chromosome.svg", device = "png")
```

## Full tutorial
https://openbiox.github.io/Bizard/Omics/CollinearityPlot.html
