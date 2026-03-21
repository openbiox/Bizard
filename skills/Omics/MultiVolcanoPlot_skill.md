# Skill: Multiple Volcano Plot (R)

## Category
Omics

## When to Use
Multiple Volcano Plot is a graph used for differential expression analysis of high-throughput data (such as transcriptomes and proteomes). Compared with the traditional volcano plot, the multi-group volcano plot can display the results of multiple groups at the same time, making it easier to compare the consistency or specificity of differential features horizontally.

## Required R Packages
- corrplot
- scRNAtoolVis

## Minimal Reproducible Code
```r
# Load packages
library(corrplot)
library(scRNAtoolVis)

# Prepare data
# Load data
data('pbmc.markers')
# View data
head(pbmc.markers)

# Create visualization
# Basic Multiple Volcano Plot
p <- jjVolcano(
  diffData = pbmc.markers,
  topGeneN = 5,
  log2FC.cutoff = 0.5,
  col.type = "updown",
  aesCol = c('#0099CC','#CC3333'),
  tile.col = corrplot::COL2('PuOr', 15)[4:12],
  cluster.order = rev(unique(pbmc.markers$cluster)),
  size  = 3.5,
  fontface = 'italic'
  )

p
```

## Key Parameters
- `position`: Position adjustment (identity, dodge, stack, fill)
- `fill`: Maps a variable to fill color for group comparison
- `color`: Maps a variable to outline/point color

## Tips
- Adjust text size with `theme(text = element_text(size = 14))` for presentations
- Include appropriate statistical thresholds (e.g., FDR < 0.05, |log2FC| > 1) in the visualization
- See the full tutorial for additional customization options and advanced examples

## Full Tutorial
https://openbiox.github.io/Bizard/Omics/MultiVolcanoPlot.html
