# Skill: Cell-Cell Communication Circle Plot (R)

## Category
Omics

## When to use
The Cell-Cell Communication Circle Plot (细胞-细胞通讯网络圈图) is a specialized visualization for depicting intercellular signaling interactions inferred from single-cell RNA sequencing (scRNA-seq) data. Using the **CellChat** R package, this plot presents a circular network where nodes represent cell populations (cell types or clusters) and directed edges indicate the strength and direction of ligand-receptor communication signals between them.

## Required R packages
- BiocManager
- CellChat
- Seurat
- circlize
- ggplot2
- igraph
- remotes

## Minimal reproducible code
```r
par(mfrow = c(1, 2), xpd = TRUE)

# Panel A: Number of interactions
netVisual_circle(
  net_count,
  vertex.weight = group_size,
  weight.scale  = TRUE,
  label.edge    = FALSE,
  title.name    = "Number of interactions"
)

# Panel B: Interaction strength (weights)
netVisual_circle(
  net_weight,
  vertex.weight = group_size,
  weight.scale  = TRUE,
  label.edge    = FALSE,
  title.name    = "Interaction weights/strength"
)

par(mfrow = c(1, 1))
```

## Full tutorial
https://openbiox.github.io/Bizard/Omics/CellChatCirclePlot.html
