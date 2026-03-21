# Skill: Grouped and Stacked Barplot (R)

## Category
Composition

## When to Use
Grouped bar charts, or clustered bar charts, extend the functionality of univariate or single-category bar charts to multivariate bar charts. In these charts, bars are grouped according to their categories, and colors represent distinguishing factors for other categorical variables. The bars are positioned to cater to a group or primary group, with colors representing secondary categories. Grouped bar charts are particularly suitable for displaying the distribution of multiple groups of categ...

## Required R Packages
- RColorBrewer
- dplyr
- ggplot2
- hrbrthemes
- streamgraph
- tibble
- tidyr
- viridis

## Minimal Reproducible Code
```r
# Load packages
library(RColorBrewer)
library(dplyr)
library(ggplot2)
library(hrbrthemes)
library(streamgraph)
library(tibble)

# Prepare data
# iris
iris_means <- iris %>%
  group_by(Species) %>%
  summarise(
    mean_sepal_length = mean(Sepal.Length),
    mean_sepal_width = mean(Sepal.Width),
    mean_petal_length = mean(Petal.Length),
    mean_petal_width = mean(Petal.Width)
  ) # Calculate the mean of the four columns for each species.

iris_means_long <- iris_means %>%
  pivot_longer(
    cols = starts_with("mean"),
    names_to = "Measurement",
    values_to = "Value"
  )

iris_means_df <- as.data.frame(iris_means) %>%
  column_to_rownames(var = "Species")
iris_matrix <- as.matrix(iris_means_df)
iris_percentage <- apply(iris_matrix, 2, function(x) { x * 100 / sum(x, na.rm = TRUE) })

# TCGA-CHOL.methylation450
TCGA_methylation <- readr::read_tsv("https://bizard-1301043367.cos.ap-guangzhou.myqcloud.com/TCGA-CHOL.methylation450_.tsv")

methylation_subset <- TCGA_methylation[c(5:9),c(4:13)]
methylation_subset <- as.data.frame(methylation_subset)
rownames(methylation_subset) <- c("cg236", "cg289", "cg292", "cg321", "cg363")
colnames(methylation_subset) <- substr(colnames(methylation_subset), 9, 12)

methylation_long <- methylation_subset %>%
  rownames_to_column(var = "Composite") %>%
  pivot_longer(cols = -Composite, names_to = "sample", values_to = "value")

methylation_long$sample <- as.numeric(factor(methylation_long$sample, levels = unique(methylation_long$sample))) # Convert the sample column to ordered values.

# TCGA-STAD.star_counts
TCGA_star <- readr::read_tsv("https://bizard-1301043367.cos.ap-guangzhou.myqcloud.com/TCGA-STAD.star_counts.tsv")

selected_rows <- TCGA_star[TCGA_star$Ensembl_ID %in% c("ENSG00000141510.18", 
                                                 "ENSG00000141736.14", 
# ... (see full tutorial for more)
```

## Key Parameters
- `fill`: Maps `Measurement` to the fill aesthetic
- `y`: Maps `Mean_Value` to the y aesthetic
- `x`: Maps `Gene` to the x aesthetic
- `width`: Controls element width
- `position`: Position adjustment (identity, dodge, stack, fill)
- `stat`: Statistical transformation to use
- `theme`: Plot theme; tutorial uses `theme_minimal()`

## Tips
- Use `theme_minimal()` or `theme_bw()` for clean, publication-ready plots
- Customize color scales with `scale_fill_manual()` or `scale_color_brewer()`
- Use `facet_wrap()` or `facet_grid()` to create multi-panel plots by group
- Ensure proportions sum to 100% and consider using a colorblind-friendly palette

## Full Tutorial
https://openbiox.github.io/Bizard/Composition/GroupedBarplot.html
