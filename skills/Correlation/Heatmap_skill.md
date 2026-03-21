# Skill: Heatmap (R)

## Category
Correlation

## When to Use
A heatmap is a powerful visualization tool that represents matrix values through color gradients. It is widely used to illustrate gene expression differences across sample groups, variations in compound concentrations, and pairwise sample similarities. More broadly, any tabular dataset can be structured into a heatmap to enhance interpretability.

## Required R Packages
- ComplexHeatmap
- RColorBrewer
- circlize
- cowplot
- d3heatmap
- dplyr
- ggplot2
- gridExtra
- heatmaply
- hrbrthemes
- htmlwidgets
- lattice
- pheatmap
- plotly
- readr
- tibble
- tidyr
- tidyverse
- viridis

## Minimal Reproducible Code
```r
# Load packages
library(ComplexHeatmap)
library(RColorBrewer)
library(circlize)
library(cowplot)
library(d3heatmap)
library(dplyr)

# Prepare data
# load built-in R datasets `mtcars`
data("mtcars", package = "datasets")
mtcars_matrix <- as.matrix(mtcars)

# Load and process methylation data
raw_methylation_data <- readr::read_tsv("https://bizard-1301043367.cos.ap-guangzhou.myqcloud.com/TCGA-CHOL.methylation450.tsv")

# Convert to matrix and clean up row/column names
methylation_matrix <- raw_methylation_data[, -1] %>%
  as.data.frame() %>%
  `rownames<-`(raw_methylation_data$Composite)

# Tidy to long format
methylation_long <- methylation_matrix %>%
  rownames_to_column("Composite") %>%
  pivot_longer(cols = -Composite, names_to = "Sample", values_to = "Methylation_Level") %>%
  mutate(
    Methylation_Level = as.numeric(Methylation_Level),
    Composite = gsub("^cg0+", "cg", Composite),
    Sample = substr(Sample, 9, 12)
  )

# Standardize methylation values
methylation_long_standardized <- methylation_long %>%
  group_by(Composite) %>%
  mutate(Standardized_Level = scale(Methylation_Level)[,1]) %>%
  ungroup()

# Convert back to wide format matrix
standardized_methylation_matrix <- methylation_long_standardized %>%
  select(Composite, Sample, Standardized_Level) %>%
  pivot_wider(names_from = Sample, values_from = Standardized_Level) %>%
  column_to_rownames("Composite") %>%
  as.matrix()


# Clean up raw methylation matrix (numerical version for raw heatmap)
methylation_matrix_num <- methylation_matrix %>%
  mutate(across(everything(), ~ as.numeric(as.character(.)))) %>%
  `rownames<-`(gsub("^cg0+", "cg", rownames(.))) %>%
  { `colnames<-`(., substr(colnames(.), 9, 12)) } %>%
# ... (see full tutorial for more)
```

## Key Parameters
- `x`: Maps `Sample` to the x aesthetic
- `y`: Maps `Composite` to the y aesthetic
- `fill`: Maps `Standardized_Level` to the fill aesthetic
- `width`: Controls element width
- `position`: Position adjustment (identity, dodge, stack, fill)
- `theme`: Plot theme; tutorial uses `theme_ipsum()`

## Tips
- Customize color scales with `scale_fill_manual()` or `scale_color_brewer()`
- Adjust text size with `theme(text = element_text(size = 14))` for presentations
- Always check and report the correlation coefficient and p-value alongside visual patterns

## Full Tutorial
https://openbiox.github.io/Bizard/Correlation/Heatmap.html
