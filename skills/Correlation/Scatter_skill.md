# Skill: Scatter Plot (R)

## Category
Correlation

## When to Use
A scatter plot is a basic visualization chart used to represent the general trend of the dependent variable changing with the independent variable.

## Required R Packages
- dplyr
- geomtextpath
- ggExtra
- ggplot2
- ggpmisc
- ggpubr
- plotly

## Minimal Reproducible Code
```r
# Load packages
library(dplyr)
library(geomtextpath)
library(ggExtra)
library(ggplot2)
library(ggpmisc)
library(ggpubr)

# Prepare data
# 1.Load iris data
data <- iris

# 2.Load gene expression data (first two rows)
data_counts <- read.csv("https://bizard-1301043367.cos.ap-guangzhou.myqcloud.com/GSE243555_all_genes_with_counts.txt", sep = "\t", header = TRUE, nrows = 10) 

axis_names <- data_counts[c(1, 2), 1]        # Save names
data_counts <- data_counts %>%      
  select(-1) %>%               # Remove first column
  slice(1:2) %>%               # remain the first two rows
  t()  %>%                     # Transpose
  as.data.frame() %>%
  setNames(c("V1", "V2"))      # Set column names

head(data_counts)

# Create visualization
# Basic plotting
p <- ggplot(data, aes(x = Sepal.Width, y = Sepal.Length)) +
  geom_point()

p
```

## Key Parameters
- `x`: Maps `Sepal` to the x aesthetic
- `y`: Maps `Sepal` to the y aesthetic
- `color`: Maps `Species` to the color aesthetic
- `shape`: Maps `Species` to the shape aesthetic
- `alpha`: Controls transparency (0 = fully transparent, 1 = opaque)
- `width`: Controls element width
- `position`: Position adjustment (identity, dodge, stack, fill)
- `stat`: Statistical transformation to use

## Tips
- Adjust text size with `theme(text = element_text(size = 14))` for presentations
- Always check and report the correlation coefficient and p-value alongside visual patterns
- See the full tutorial for additional customization options and advanced examples

## Full Tutorial
https://openbiox.github.io/Bizard/Correlation/Scatter.html
