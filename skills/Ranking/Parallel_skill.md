# Skill: Parallel Coordinates Plot (R)

## Category
Ranking

## When to Use
Parallel coordinate plots are a common method for visualizing high-dimensional multivariate data. To display a set of objects in a multidimensional space, multiple parallel and equally spaced axes are drawn, and the objects in the multidimensional space are represented as broken lines with vertices on the parallel axes. Although parallel line plots are a special type of line plot, they differ significantly from ordinary line plots. This is because parallel line plots are not limited to descri...

## Required R Packages
- GGally
- MASS
- RColorBrewer
- dplyr
- ggbump
- hrbrthemes
- patchwork
- tibble
- tidyr
- viridis

## Minimal Reproducible Code
```r
# Load packages
library(GGally)
library(MASS)
library(RColorBrewer)
library(dplyr)
library(ggbump)
library(hrbrthemes)

# Prepare data
# iris
data_iris <- iris
data_iris <- data_iris %>%
  group_by(Species) %>%
  sample_n(size = 20, replace = FALSE)

# TCGA-CHOL.methylation450
methylation_raw <- readr::read_tsv("https://bizard-1301043367.cos.ap-guangzhou.myqcloud.com/TCGA-CHOL.methylation450_.tsv")
methylation_selected <- methylation_raw[c(5,7,11),c(4:6)]
rownames(methylation_selected) <- c("cg236", "cg292", "cg658")
colnames(methylation_selected) <- substr(colnames(methylation_selected), 9, 12)
data_tcga <- methylation_selected %>%
  rownames_to_column(var = "Composite") %>%
  pivot_longer(cols = -Composite, names_to = "sample", values_to = "value")
data_tcga <- data_tcga %>%
  mutate(sample = as.numeric(factor(sample))) # Convert the sample to a numerical value

# Create visualization
# Basic parallel graph
p <- ggparcoord(data_iris, columns = 1:4, groupColumn = 5) 

p
```

## Key Parameters
- `x`: Maps `sample` to the x aesthetic
- `y`: Maps `value` to the y aesthetic
- `color`: Maps `Composite` to the color aesthetic
- `alpha`: Controls transparency (0 = fully transparent, 1 = opaque)
- `position`: Position adjustment (identity, dodge, stack, fill)
- `theme`: Plot theme; tutorial uses `theme_ipsum()`

## Tips
- Use `theme_minimal()` or `theme_bw()` for clean, publication-ready plots
- Customize color scales with `scale_fill_manual()` or `scale_color_brewer()`
- Sort categories by value rather than alphabetically for clearer ranking visualization

## Full Tutorial
https://openbiox.github.io/Bizard/Ranking/Parallel.html
