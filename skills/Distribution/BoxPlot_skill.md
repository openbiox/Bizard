# Skill: Box Plot (R)

## Category
Distribution

## When to Use
Boxplots visualize the central tendency and dispersion of one or more sets of continuous quantitative data. They incorporate statistical measures that not only compare differences across categories but also reveal dispersion, outliers, and distribution patterns.

## Required R Packages
- dplyr
- ggExtra
- ggplot2
- ggpmisc
- ggpubr
- ggtext
- hrbrthemes
- readr
- rstatix
- viridis

## Minimal Reproducible Code
```r
# Load packages
library(dplyr)
library(ggExtra)
library(ggplot2)
library(ggpmisc)
library(ggpubr)
library(ggtext)

# Prepare data
# Load mtcars dataset
data("mtcars")
data_mtcars <- mtcars

# Load mpg dataset from ggplot2 package
data_mpg <- ggplot2::mpg

# Load diamonds dataset from ggplot2 package
data_diamonds <- ggplot2::diamonds

# Load the TCGA-BRCA gene expression dataset from a processed CSV file  
data_TCGA <- readr::read_csv("https://bizard-1301043367.cos.ap-guangzhou.myqcloud.com/TCGA-BRCA.htseq_counts_processed.csv")
data_TCGA1 <- data_TCGA[1:5,] %>%
  gather(key = "sample",value = "gene_expression",3:1219)
```

## Key Parameters
- `x`: Maps `class` to the x aesthetic
- `y`: Maps `hwy` to the y aesthetic
- `fill`: Maps `type` to the fill aesthetic
- `alpha`: Maps `type` to the alpha aesthetic
- `width`: Controls element width
- `position`: Position adjustment (identity, dodge, stack, fill)
- `stat`: Statistical transformation to use
- `theme`: Plot theme; tutorial uses `theme_ipsum()`

## Tips
- Use `theme_minimal()` or `theme_bw()` for clean, publication-ready plots
- Customize color scales with `scale_fill_manual()` or `scale_color_brewer()`
- Use `facet_wrap()` or `facet_grid()` to create multi-panel plots by group
- Consider adding `geom_jitter()` or raw data points alongside distribution plots for small sample sizes

## Full Tutorial
https://openbiox.github.io/Bizard/Distribution/BoxPlot.html
