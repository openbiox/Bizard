# Skill: Bar Plot (R)

## Category
Ranking

## When to Use
A bar plot is a graph that uses the height or length of the bars to represent the amount of data.

## Required R Packages
- cowplot
- dplyr
- forcats
- ggpattern
- ggplot2
- ggpubr
- hrbrthemes
- magrittr
- palmerpenguins
- rstatix
- tidyr

## Minimal Reproducible Code
```r
# Load packages
library(cowplot)
library(dplyr)
library(forcats)
library(ggpattern)
library(ggplot2)
library(ggpubr)

# Prepare data
data_TCGA <- readr::read_csv("https://bizard-1301043367.cos.ap-guangzhou.myqcloud.com/TCGA-BRCA.htseq_counts_processed.csv")

data_TCGA1 <- data_TCGA[1:5,] %>%
  gather(key = "sample",value = "gene_expression",3:1219)

data_tcga_mean <- aggregate(data_TCGA1$gene_expression, 
                            by=list(data_TCGA1$gene_name), mean) # mean
colnames(data_tcga_mean) <- c("gene","expression")

data_tcga_sd <- aggregate(data_TCGA1$gene_expression, 
                            by=list(data_TCGA1$gene_name), sd)
colnames(data_tcga_sd) <- c("gene","sd")

data_tcga <- merge(data_tcga_mean, data_tcga_sd, by="gene")

data_penguins <- penguins

data_penguins_flipper_length <- aggregate(data_penguins$flipper_length_mm,
                                by=list(data_penguins$species,data_penguins$sex),
                                mean)
colnames(data_penguins_flipper_length) <- c("species","sex","flipper_length_mm")

data_mpg <- mpg

# Create visualization
# Basic bar plot
p <- ggplot(data_tcga_mean, aes(x=gene, y=expression)) + 
  geom_bar(stat = "identity")

p
```

## Key Parameters
- `x`: Maps `gene` to the x aesthetic
- `y`: Maps `expression` to the y aesthetic
- `fill`: Maps `group` to the fill aesthetic
- `colour`: Maps `group` to the colour aesthetic
- `alpha`: Controls transparency (0 = fully transparent, 1 = opaque)
- `width`: Controls element width
- `position`: Position adjustment (identity, dodge, stack, fill)
- `stat`: Statistical transformation to use

## Tips
- Use `theme_minimal()` or `theme_bw()` for clean, publication-ready plots
- Use `coord_flip()` for horizontal orientation when labels are long
- Customize color scales with `scale_fill_manual()` or `scale_color_brewer()`
- Sort categories by value rather than alphabetically for clearer ranking visualization

## Full Tutorial
https://openbiox.github.io/Bizard/Ranking/BarPlot.html
