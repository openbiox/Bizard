# Skill: Motif Plot (R)

## Category
Omics

## When to Use
For visualizing motif logos, ggseqlogo is an R package based on ggplot2 specifically designed for plotting logos from sequence motifs. Compared to other motif visualization tools, ggseqlogo boasts advantages such as concise syntax, flexible output formats, and full compatibility with the ggplot2 ecosystem. The package supports various sequence input formats, including position-frequency matrices (PFM), position-weight matrices (PWM), and sequence vectors, and provides rich customization optio...

## Required R Packages
- cowplot
- ggplot2
- ggseqlogo
- gridExtra

## Minimal Reproducible Code
```r
# Load packages
library(cowplot)
library(ggplot2)
library(ggseqlogo)
library(gridExtra)

# Prepare data
data(ggseqlogo_sample)

head(pfms_dna,n = 1)

head(seqs_aa, n = 1)[[1]][1:3]

# Create visualization
# Using sequence vectors
ggseqlogo(seqs_dna$MA0001.1)
# Using PFM matrix
ggseqlogo(pfms_dna$MA0018.2)
# Plotting using ggplot syntax
ggplot() + geom_logo( seqs_dna$MA0001.1 ) + theme_logo()
```

## Key Parameters
- `color`: Maps `mut` to the color aesthetic
- `size`: Maps `mut` to the size aesthetic
- `alpha`: Controls transparency (0 = fully transparent, 1 = opaque)
- `width`: Controls element width
- `position`: Position adjustment (identity, dodge, stack, fill)
- `stat`: Statistical transformation to use
- `theme`: Plot theme; tutorial uses `theme_logo()`

## Tips
- The tutorial includes a '3. Motif plot beautify' section with advanced styling options
- Customize color scales with `scale_fill_manual()` or `scale_color_brewer()`
- Use `facet_wrap()` or `facet_grid()` to create multi-panel plots by group
- Include appropriate statistical thresholds (e.g., FDR < 0.05, |log2FC| > 1) in the visualization

## Full Tutorial
https://openbiox.github.io/Bizard/Omics/MotifPlot.html
