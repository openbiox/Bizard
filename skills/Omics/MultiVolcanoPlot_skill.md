# Skill: Multiple Volcano Plot (R)

## Category
Omics

## When to use
Multiple Volcano Plot is a graph used for differential expression analysis of high-throughput data (such as transcriptomes and proteomes). Compared with the traditional volcano plot, the multi-group volcano plot can display the results of multiple groups at the same time, making it easier to compare the consistency or specificity of differential features horizontally.

## Required R packages
- corrplot
- scRNAtoolVis

## Minimal reproducible code
```r
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

## Full tutorial
https://openbiox.github.io/Bizard/Omics/MultiVolcanoPlot.html
