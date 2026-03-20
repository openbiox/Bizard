# Skill: Gene Structure Plot (R)

## Category
Omics

## When to use
In biology, especially in molecular biology research, analyzing the expression and regulation patterns of genes has always been a research focus. In this process, it is inevitable that there will be a need to draw the structure of a gene or the upstream and downstream relationships. Therefore, this tutorial will summarize some common gene structure drawing methods based on the R package gggenes.

## Required R packages
- gggenes
- ggtree
- tidyverse

## Minimal reproducible code
```r
# Plotting the relative positions of a series of genes
ggplot(example_genes, aes(xmin = start, xmax = end, y = molecule)) +
    geom_gene_arrow() +
    facet_wrap(~ molecule, scales = "free", ncol = 1)  # gggenes is usually used with the facet_wrap function for faceting. It should be noted that if the drawing interface is too small, an error message will be displayed: "Viewport has zero dimension(s)". Just enlarge the drawing window or set a larger interface.
```

## Full tutorial
https://openbiox.github.io/Bizard/Omics/GeneStructurePlot.html
