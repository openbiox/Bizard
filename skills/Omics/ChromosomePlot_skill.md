# Skill: Chromosome Plot (R)

## Category
Omics

## When to Use
An chromosome plot (ideogram) is a graphical tool used to visualize chromosome structure and various genomic features on chromosomes. It typically represents each chromosome individually, drawing the length and structures such as the centromere to scale. Additionally, it can annotate multiple types of information on the chromosomes, including gene density, genetic variations, expression levels, repetitive sequences, and functional markers.

## Required R Packages
- RIdeogram

## Minimal Reproducible Code
```r
# Load packages
library(RIdeogram)

# Prepare data
data(human_karyotype, package="RIdeogram")
data(gene_density, package="RIdeogram")
data(Random_RNAs_500, package="RIdeogram")

# Create visualization
# Basic Chromosome Plot
ideogram(karyotype = human_karyotype)
convertSVG("chromosome.svg", device = "png")
```

## Key Parameters
- `width`: Controls element width
- `position`: Position adjustment (identity, dodge, stack, fill)
- `fill`: Maps a variable to fill color for group comparison
- `color`: Maps a variable to outline/point color

## Tips
- Adjust text size with `theme(text = element_text(size = 14))` for presentations
- Include appropriate statistical thresholds (e.g., FDR < 0.05, |log2FC| > 1) in the visualization
- See the full tutorial for additional customization options and advanced examples

## Full Tutorial
https://openbiox.github.io/Bizard/Omics/ChromosomePlot.html
