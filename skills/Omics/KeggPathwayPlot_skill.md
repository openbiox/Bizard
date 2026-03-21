# Skill: KEGG Pathway Plot (R)

## Category
Omics

## When to Use
Visualize kegg pathway plot data in a biomedical context.

## Required R Packages
- dbplyr
- pathview

## Minimal Reproducible Code
```r
p1 <- pathview(gene.data = gse16873.d[, 1], # Input gene matrix
               pathway.id = "04110", # Pathway ID
               species = "hsa", # Species: Human
               out.suffix = "gse16873_KEGG", # Output file suffix
               kegg.native = T, # Output in original KEGG view
               same.layer = T # Drawing a single layer
               )
p1
```

## Full Tutorial
https://openbiox.github.io/Bizard/Omics/KeggPathwayPlot.html
