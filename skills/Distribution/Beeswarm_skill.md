# Skill: Beeswarm Plot (R)

## Category
Distribution

## When to Use
A beeswarm plot disperses data points slightly to prevent overlap, making distribution density and trends clearer. It is especially useful for visualizing categorical data in small datasets. This section presents examples using R and the `beeswarm` and `ggbeeswarm` packages.

## Required R Packages
- beeswarm
- ggbeeswarm
- ggsignif
- plyr
- readr
- tidyverse

## Minimal Reproducible Code
```r
# Load packages
library(beeswarm)
library(ggbeeswarm)
library(ggsignif)
library(plyr)
library(readr)
library(tidyverse)

# Prepare data
# Load iris dataset
data("iris")

# Load the TCGA-LIHC gene expression dataset from a processed CSV file  
TCGA_gene_expression <- read.csv("https://bizard-1301043367.cos.ap-guangzhou.myqcloud.com/TCGA-LIHC.star_fpkm_processed.csv")

# Load the TCGA-LIHC clinical info dataset
TCGA_clinic <- read.csv("https://bizard-1301043367.cos.ap-guangzhou.myqcloud.com/TCGA.LIHC.clinicalMatrix.csv") %>%
  mutate(T = as.factor(T))

#Prepare Statistics data
data_summary <- function(data, varname, groupnames) {
  summary_func <- function(x, col) {
    c(mean = mean(x[[col]], na.rm = TRUE),
      sd = sd(x[[col]], na.rm = TRUE))
  }
  data_sum <- ddply(data, groupnames, .fun = summary_func, varname)
  
  data_sum <- rename_with(data_sum, ~ varname, "mean")
  
  return(data_sum)
}
iris_sum <- data_summary(iris, varname="Sepal.Length", groupnames="Species")
TCGA_gene_sum <- data_summary(TCGA_gene_expression, varname="gene_expression", groupnames="sample")

# Create visualization
p1 <- beeswarm(iris$Sepal.Length)
```

## Key Parameters
- `y`: Maps `Sepal` to the y aesthetic
- `x`: Maps `Species` to the x aesthetic
- `colour`: Maps `T` to the colour aesthetic
- `fill`: Maps `Species` to the fill aesthetic
- `alpha`: Controls transparency (0 = fully transparent, 1 = opaque)
- `width`: Controls element width
- `position`: Position adjustment (identity, dodge, stack, fill)
- `stat`: Statistical transformation to use

## Tips
- The tutorial includes a '8. Customization of the Beeswarm Plot (`ggbeeswarm` package)' section with advanced styling options
- Use `theme_minimal()` or `theme_bw()` for clean, publication-ready plots
- Customize color scales with `scale_fill_manual()` or `scale_color_brewer()`
- Consider adding `geom_jitter()` or raw data points alongside distribution plots for small sample sizes

## Full Tutorial
https://openbiox.github.io/Bizard/Distribution/Beeswarm.html
