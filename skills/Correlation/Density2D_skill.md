# Skill: 2D Density (R)

## Category
Correlation

## When to Use
A 2D density plot shows the distribution of a combination of two numerical variables, using color gradients (or contour lines) to indicate the number of observations within an area. This can be used to identify trends in a dataset and analyze relationships between two variables. Scatter plots can be difficult to interpret when displaying large datasets because the points overlap and cannot be individually distinguished. In these cases, a two-dimensional density plot is useful.

## Required R Packages
- MASS
- RColorBrewer
- ggplot2
- hexbin
- mvtnorm
- patchwork
- plotly

## Minimal Reproducible Code
```r
# Load packages
library(MASS)
library(RColorBrewer)
library(ggplot2)
library(hexbin)
library(mvtnorm)
library(patchwork)

# Prepare data
# mtcars
data_mtcars <- mtcars[, c("mpg", "hp")]

# TCGA-BRCA.star_counts
tcga_raw <- readr::read_tsv("https://bizard-1301043367.cos.ap-guangzhou.myqcloud.com/TCGA-BRCA.star_counts.tsv")

tcga_tp53_mdm2 <- tcga_raw[tcga_raw$Ensembl_ID %in% c("ENSG00000141510.18", "ENSG00000131747.15"), ]
TP53_values <- tcga_tp53_mdm2[1, -1]
MDM2_values <- tcga_tp53_mdm2[2, -1]

data_tcga <- data.frame(TP53 = as.numeric(TP53_values),
                        MDM2 = as.numeric(MDM2_values))

# Create visualization
# 2D Histogram
p <- ggplot(data_tcga, aes(x = TP53, y = MDM2)) +
  geom_bin2d() +
  labs(fill = "Gene_expression\n(STAR_counts)") + 
  theme_bw()

p
```

## Key Parameters
- `x`: Maps `TP53` to the x aesthetic
- `y`: Maps `MDM2` to the y aesthetic
- `position`: Position adjustment (identity, dodge, stack, fill)
- `stat`: Statistical transformation to use
- `theme`: Plot theme; tutorial uses `theme_bw()`

## Tips
- Use `theme_minimal()` or `theme_bw()` for clean, publication-ready plots
- Customize color scales with `scale_fill_manual()` or `scale_color_brewer()`
- Always check and report the correlation coefficient and p-value alongside visual patterns

## Full Tutorial
https://openbiox.github.io/Bizard/Correlation/Density2D.html
