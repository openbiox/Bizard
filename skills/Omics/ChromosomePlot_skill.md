# Skill: Chromosome Plot (R)

## Category
Omics

## When to use
An chromosome plot (ideogram) is a graphical tool used to visualize chromosome structure and various genomic features on chromosomes. It typically represents each chromosome individually, drawing the length and structures such as the centromere to scale. Additionally, it can annotate multiple types of information on the chromosomes, including gene density, genetic variations, expression levels, repetitive sequences, and functional markers.

## Required R packages
- RIdeogram

## Minimal reproducible code
```r
# Basic Chromosome Plot
ideogram(karyotype = human_karyotype)
convertSVG("chromosome.svg", device = "png")
```

## Full tutorial
https://openbiox.github.io/Bizard/Omics/ChromosomePlot.html
