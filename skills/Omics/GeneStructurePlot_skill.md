# Skill: Gene Structure Plot (R)

## Category
Omics

## When to Use
In biology, especially in molecular biology research, analyzing the expression and regulation patterns of genes has always been a research focus. In this process, it is inevitable that there will be a need to draw the structure of a gene or the upstream and downstream relationships. Therefore, this tutorial will summarize some common gene structure drawing methods based on the R package gggenes.

## Required R Packages
- gggenes
- ggtree
- tidyverse

## Minimal Reproducible Code
```r
# Load packages
library(gggenes)
library(ggtree)
library(tidyverse)

# Prepare data
head(example_genes)

# Create visualization
# Plotting the relative positions of a series of genes
ggplot(example_genes, aes(xmin = start, xmax = end, y = molecule)) +
    geom_gene_arrow() +
    facet_wrap(~ molecule, scales = "free", ncol = 1)  # gggenes is usually used with the facet_wrap function for faceting. It should be noted that if the drawing interface is too small, an error message will be displayed: "Viewport has zero dimension(s)". Just enlarge the drawing window or set a larger interface.
```

## Key Parameters
- `y`: Maps `molecule` to the y aesthetic
- `fill`: Maps `gene` to the fill aesthetic
- `x`: Maps `position` to the x aesthetic
- `alpha`: Controls transparency (0 = fully transparent, 1 = opaque)
- `width`: Controls element width
- `position`: Position adjustment (identity, dodge, stack, fill)
- `theme`: Plot theme; tutorial uses `theme_genes()`

## Tips
- The tutorial includes a '2. Beautification' section with advanced styling options
- Customize color scales with `scale_fill_manual()` or `scale_color_brewer()`
- Use `facet_wrap()` or `facet_grid()` to create multi-panel plots by group
- Include appropriate statistical thresholds (e.g., FDR < 0.05, |log2FC| > 1) in the visualization

## Full Tutorial
https://openbiox.github.io/Bizard/Omics/GeneStructurePlot.html
