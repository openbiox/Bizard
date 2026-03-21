# Skill: Cell-Cell Communication Circle Plot (R)

## Category
Omics

## When to Use
The Cell-Cell Communication Circle Plot (细胞-细胞通讯网络圈图) is a specialized visualization for depicting intercellular signaling interactions inferred from single-cell RNA sequencing (scRNA-seq) data. Using the **CellChat** R package, this plot presents a circular network where nodes represent cell populations (cell types or clusters) and directed edges indicate the strength and direction of ligand-receptor communication signals between them.

## Required R Packages
- BiocManager
- CellChat
- Seurat
- circlize
- ggplot2
- igraph
- remotes

## Minimal Reproducible Code
```r
# Load packages
library(BiocManager)
library(CellChat)
library(Seurat)
library(circlize)
library(ggplot2)
library(igraph)

# Prepare data
# Simulate a communication count/weight matrix
# In real use, these come from cellchat@net$count and cellchat@net$weight

set.seed(42)
cell_types <- c("CD4 T", "CD8 T", "NK", "B cell", "Monocyte",
                "DC", "Fibroblast", "Epithelial", "Endothelial")
n <- length(cell_types)

net_count  <- matrix(sample(0:50, n * n, replace = TRUE), n, n,
                     dimnames = list(cell_types, cell_types))
net_weight <- matrix(runif(n * n, 0, 1), n, n,
                     dimnames = list(cell_types, cell_types))
diag(net_count)  <- 0  # remove self-communication
diag(net_weight) <- 0

# Group size (proportional to number of cells per type, simulated)
group_size <- sample(50:500, n, replace = TRUE)
names(group_size) <- cell_types

cat("Simulated", n, "cell types with", sum(net_count), "total interactions\n")
print(net_count)

# Create visualization
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
# ... (see full tutorial for more)
```

## Key Parameters
- `x`: Maps `Receiver` to the x aesthetic
- `y`: Maps `Sender` to the y aesthetic
- `fill`: Maps `Weight` to the fill aesthetic
- `width`: Controls element width

## Tips
- Use `theme_minimal()` or `theme_bw()` for clean, publication-ready plots
- Customize color scales with `scale_fill_manual()` or `scale_color_brewer()`
- Include appropriate statistical thresholds (e.g., FDR < 0.05, |log2FC| > 1) in the visualization

## Full Tutorial
https://openbiox.github.io/Bizard/Omics/CellChatCirclePlot.html
