# Skill: Manhattan Plot (R)

## Category
Omics

## When to Use
Manhattan plot is a graph used to describe the relationship between mutations on chromosomes and traits. It is named Manhattan plot because it resembles the urban landscape of Manhattan, USA. Manhattan plot is generally drawn in the form of scatter plot, but it can also be displayed in bar chart or line chart. It is usually drawn using R package qqman or directly using ggplot2.

## Required R Packages
- aplot
- qqman
- tidyverse

## Minimal Reproducible Code
```r
# Load packages
library(aplot)
library(qqman)
library(tidyverse)

# Prepare data
# View the dataset
head(gwasResults)

# Create visualization
# Basic manhattan plot
manhattan(gwasResults)
```

## Key Parameters
- `x`: Maps `BP` to the x aesthetic
- `alpha`: Controls transparency (0 = fully transparent, 1 = opaque)
- `position`: Position adjustment (identity, dodge, stack, fill)
- `theme`: Plot theme; tutorial uses `theme_bw()`

## Tips
- Use `theme_minimal()` or `theme_bw()` for clean, publication-ready plots
- Customize color scales with `scale_fill_manual()` or `scale_color_brewer()`
- Include appropriate statistical thresholds (e.g., FDR < 0.05, |log2FC| > 1) in the visualization

## Full Tutorial
https://openbiox.github.io/Bizard/Omics/ManhattanPlot.html
