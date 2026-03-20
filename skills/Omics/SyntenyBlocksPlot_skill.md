# Skill: Synteny Blocks Plot (R)

## Category
Omics

## When to use
Collinearity is widely used in the study of complex genomes. This tutorial, based on the R package syntR, summarizes the identification of shared collinearity blocks between two genetic maps, chromosomal rearrangements, and their mapping.

## Required R packages
- syntR

## Minimal reproducible code
```r
# Adjust the order of the graphs
plot_maps(map_df = map_list[[1]], map1_chrom_breaks = map_list[[2]], map2_chrom_breaks = map_list[[3]])
```

## Full tutorial
https://openbiox.github.io/Bizard/Omics/SyntenyBlocksPlot.html
