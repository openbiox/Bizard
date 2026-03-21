# Skill: GWAS Circos Plot (R)

## Category
Omics

## When to Use
The visualization of Genome-Wide Association Study (GWAS) results mainly includes SNP circular plots displayed by chromosome positions, SNP density plots, Manhattan plots for significance screening, QQ plots comparing the distribution of observed p-values with expected p-values, etc., which are used to screen candidate variant genes at the genome-wide level.

## Required R Packages
- CMplot

## Minimal Reproducible Code
```r
# Load packages
library(CMplot)

# Prepare data
# Example data
data(pig60K)
data <- pig60K

# Data preview
head(data, 5)

# Create visualization
# SNP screening genome circular map
CMplot(
  data,
  type = "p",
  plot.type = "c",
  chr.labels = paste("Chr", c(1:18, "X", "Y"), sep = ""),
  r = 8,
  cir.axis = TRUE,
  outward = TRUE,
  cir.axis.col = "black",
  cir.chr.h = 2,
  chr.den.col = "black",
  file.output = FALSE,
  verbose = FALSE,
  mar = c(0,0,0,0)
)
```

## Key Parameters
- `fill`: Maps a variable to fill color for group comparison
- `color`: Maps a variable to outline/point color

## Tips
- Adjust text size with `theme(text = element_text(size = 14))` for presentations
- Include appropriate statistical thresholds (e.g., FDR < 0.05, |log2FC| > 1) in the visualization
- See the full tutorial for additional customization options and advanced examples

## Full Tutorial
https://openbiox.github.io/Bizard/Omics/GwasSnpPlot.html
