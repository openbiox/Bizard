# Skill: Violin Plot (R)

## Category
Distribution

## When to Use
A violin plot combines elements of a density plot and a box plot to visualize data distribution. It displays key statistical information, including the median, quartiles, minimum, and maximum values. Violin plots are particularly useful for comparing distributions across different groups, offering a more intuitive representation than traditional box plots by revealing the shape of the data distribution.

## Required R Packages
- dplyr
- forcats
- gghalves
- ggplot2
- ggpubr
- ggstatsplot
- hrbrthemes
- palmerpenguins
- readr
- tidyr
- viridis

## Minimal Reproducible Code
```r
# Load packages
library(dplyr)
library(forcats)
library(gghalves)
library(ggplot2)
library(ggpubr)
library(ggstatsplot)

# Prepare data
# Load the TCGA-BRCA gene expression dataset from a processed CSV file  
data_counts <- readr::read_csv("https://bizard-1301043367.cos.ap-guangzhou.myqcloud.com/TCGA-BRCA.htseq_counts_processed.csv")

# Load built-in R dataset iris
data_wide <- iris[ , 1:4] # Take the data in columns 1-4 of the iris database as an example

# Load built-in R dataset penguins
data("penguins", package = "palmerpenguins")
data_penguins <- drop_na(penguins) # Remove missing values

# Manually create a demonstration dataset with grouped values 
data <- data.frame(
  name=c( rep("A",500), rep("B",500), rep("B",500), rep("C",20), rep('D', 100)  ),
  value=c( rnorm(500, 10, 5), rnorm(500, 13, 1), rnorm(500, 18, 1), rnorm(20, 25, 4), rnorm(100, 12, 1) )
  )
sample_size <- data %>% 
  group_by(name) %>% 
  summarize(num=n()) # Compute the sample size for each group

# Create visualization
# Basic Violin Plot
p <- ggplot(data, aes(x=name, y=value, fill=name)) + 
  geom_violin()

p
```

## Key Parameters
- `x`: Maps `gene_name` to the x aesthetic
- `y`: Maps `gene_expression` to the y aesthetic
- `fill`: Maps `gene_name` to the fill aesthetic
- `color`: Maps `gene_name` to the color aesthetic
- `alpha`: Controls transparency (0 = fully transparent, 1 = opaque)
- `width`: Controls element width
- `position`: Position adjustment (identity, dodge, stack, fill)
- `stat`: Statistical transformation to use

## Tips
- Use `coord_flip()` for horizontal orientation when labels are long
- Customize color scales with `scale_fill_manual()` or `scale_color_brewer()`
- Consider adding `geom_jitter()` or raw data points alongside distribution plots for small sample sizes

## Full Tutorial
https://openbiox.github.io/Bizard/Distribution/ViolinPlot.html
