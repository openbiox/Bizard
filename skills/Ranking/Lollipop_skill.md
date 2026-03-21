# Skill: Lollipop Plot (R)

## Category
Ranking

## When to Use
A lollipop plot is a variation of a bar chart and a scatter plot. It consists of a line segment and a point, which can clearly display data while reducing the amount of graphics. At the same time, the lollipop plot can help align values with categories and is very suitable for comparing the differences between values of multiple categories.

## Required R Packages
- cowplot
- ggalt
- ggplot2
- ggpubr
- hrbrthemes
- palmerpenguins
- rstatix
- tidyr

## Minimal Reproducible Code
```r
# Load packages
library(cowplot)
library(ggalt)
library(ggplot2)
library(ggpubr)
library(hrbrthemes)
library(palmerpenguins)

# Prepare data
data_TCGA <- read.csv("https://bizard-1301043367.cos.ap-guangzhou.myqcloud.com/TCGA-BRCA.htseq_counts_processed.csv")

data_TCGA1 <- data_TCGA[1:25,] %>%
  gather(key = "sample",value = "gene_expression",3:1219)

data_tcga_mean <- aggregate(data_TCGA1$gene_expression, 
                            by=list(data_TCGA1$gene_name), mean) # mean
colnames(data_tcga_mean) <- c("gene","expression")

data_tcga_sd <- aggregate(data_TCGA1$gene_expression, 
                          by=list(data_TCGA1$gene_name), sd)
colnames(data_tcga_sd) <- c("gene","sd")

data_tcga <- merge(data_tcga_mean, data_tcga_sd, by="gene")

data_penguins <- penguins

data_penguins_mean <- aggregate(data_penguins$flipper_length_mm, 
                                by=list(data_penguins$species,data_penguins$sex), 
                                mean)
colnames(data_penguins_mean) <- c("species","sex","flipper_length")

# Convert long format data to wide format
data_penguins_mean <- spread(data_penguins_mean, key="sex", value="flipper_length")

row_mean = apply(data_penguins_mean[,2:3],1,mean)

data_penguins_mean$mean <- row_mean

# Create visualization
# `TCGA` data
p <- ggplot(data_tcga, aes(x=gene, y=expression)) +
  geom_point() + 
  geom_segment( aes(x=gene, xend=gene, y=0, yend=expression)) +
  theme(axis.text.x = element_text(angle = 30,vjust = 0.85,hjust = 0.85))

p
```

## Key Parameters
- `x`: Maps `mean` to the x aesthetic
- `y`: Maps `species` to the y aesthetic
- `size`: Maps `mean` to the size aesthetic
- `alpha`: Controls transparency (0 = fully transparent, 1 = opaque)
- `position`: Position adjustment (identity, dodge, stack, fill)
- `stat`: Statistical transformation to use
- `theme`: Plot theme; tutorial uses `theme_light()`

## Tips
- The tutorial includes a '2. Customize appearance' section with advanced styling options
- Use `coord_flip()` for horizontal orientation when labels are long
- Sort categories by value rather than alphabetically for clearer ranking visualization

## Full Tutorial
https://openbiox.github.io/Bizard/Ranking/Lollipop.html
