# Skill: Synteny Blocks Plot (R)

## Category
Omics

## When to Use
Collinearity is widely used in the study of complex genomes. This tutorial, based on the R package syntR, summarizes the identification of shared collinearity blocks between two genetic maps, chromosomal rearrangements, and their mapping.

## Required R Packages
- syntR

## Minimal Reproducible Code
```r
# Load packages
library(syntR)

# Prepare data
# load the example marker data
data(ann_pet_map)
head(ann_pet_map)

# Create visualization
# Adjust the order of the graphs
plot_maps(map_df = map_list[[1]], map1_chrom_breaks = map_list[[2]], map2_chrom_breaks = map_list[[3]])
```

## Key Parameters
- `stat`: Statistical transformation to use
- `fill`: Maps a variable to fill color for group comparison
- `color`: Maps a variable to outline/point color

## Tips
- Adjust text size with `theme(text = element_text(size = 14))` for presentations
- Include appropriate statistical thresholds (e.g., FDR < 0.05, |log2FC| > 1) in the visualization
- See the full tutorial for additional customization options and advanced examples

## Full Tutorial
https://openbiox.github.io/Bizard/Omics/SyntenyBlocksPlot.html
