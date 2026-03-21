# Skill: Collinearity Plot (R)

## Category
Omics

## When to Use
Collinearity plot is often used to compare genome sequences of different species, identify conserved homologous gene blocks and their arrangement order, and reveal changes in chromosome structure during evolution. This plot is widely used in the study of genome evolution, functional gene localization, and species relationship analysis.

## Required R Packages
- RIdeogram

## Minimal Reproducible Code
```r
# Load packages
library(RIdeogram)

# Prepare data
data(karyotype_ternary_comparison, package="RIdeogram")
data(synteny_ternary_comparison, package="RIdeogram")

# Create visualization
# Basic Collinearity Plot
ideogram(karyotype = karyotype_ternary_comparison, synteny = synteny_ternary_comparison)
convertSVG("chromosome.svg", device = "png")
```

## Key Parameters
- `fill`: Maps a variable to fill color for group comparison
- `color`: Maps a variable to outline/point color

## Tips
- Adjust text size with `theme(text = element_text(size = 14))` for presentations
- Include appropriate statistical thresholds (e.g., FDR < 0.05, |log2FC| > 1) in the visualization
- See the full tutorial for additional customization options and advanced examples

## Full Tutorial
https://openbiox.github.io/Bizard/Omics/CollinearityPlot.html
