# Skill: KEGG Pathway Plot (R)

## Category
Omics

## When to Use
Create a KEGG Pathway Plot visualization in R for biomedical data analysis and research publications.

## Required R Packages
- dbplyr
- pathview

## Minimal Reproducible Code
```r
# Load packages
library(dbplyr)
library(pathview)

# Prepare data
data("gse16873.d")
head(gse16873.d)
gene_data <- as.data.frame(gse16873.d)
head(gene_data)

# Create visualization
p1 <- pathview(gene.data = gse16873.d[, 1], # Input gene matrix
               pathway.id = "04110", # Pathway ID
               species = "hsa", # Species: Human
               out.suffix = "gse16873_KEGG", # Output file suffix
               kegg.native = T, # Output in original KEGG view
               same.layer = T # Drawing a single layer
               )
p1
```

## Key Parameters
- `position`: Position adjustment (identity, dodge, stack, fill)
- `stat`: Statistical transformation to use
- `fill`: Maps a variable to fill color for group comparison
- `color`: Maps a variable to outline/point color

## Tips
- Adjust text size with `theme(text = element_text(size = 14))` for presentations
- Include appropriate statistical thresholds (e.g., FDR < 0.05, |log2FC| > 1) in the visualization
- See the full tutorial for additional customization options and advanced examples

## Full Tutorial
https://openbiox.github.io/Bizard/Omics/KeggPathwayPlot.html
