# Skill: GWAS Circos Plot (R)

## Category
Omics

## When to use
The visualization of Genome-Wide Association Study (GWAS) results mainly includes SNP circular plots displayed by chromosome positions, SNP density plots, Manhattan plots for significance screening, QQ plots comparing the distribution of observed p-values with expected p-values, etc., which are used to screen candidate variant genes at the genome-wide level.

## Required R packages
- CMplot

## Minimal reproducible code
```r
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

## Full tutorial
https://openbiox.github.io/Bizard/Omics/GwasSnpPlot.html
