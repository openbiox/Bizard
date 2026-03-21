# Skill: ComplexHeatmap (R)

## Category
Correlation

## When to Use
Create a ComplexHeatmap visualization in R for biomedical data analysis and research publications.

## Required R Packages
- ComplexHeatmap
- circlize
- dendextend
- gridExtra
- pheatmap
- tidyr

## Minimal Reproducible Code
```r
# Load packages
library(ComplexHeatmap)
library(circlize)
library(dendextend)
library(gridExtra)
library(pheatmap)
library(tidyr)

# Prepare data
# data_mat (continuous)
data <- readr::read_csv("https://bizard-1301043367.cos.ap-guangzhou.myqcloud.com/TCGA.BRCA.sampleMap_HumanMethylation27_ch.csv")

data <- as.data.frame(data)
rownames(data) <- data[,1]
data <- data[,-1]

data_TCGA <- na.omit(data[1:20,1:20])

data_TCGA <- as.matrix(data_TCGA)  

# Discrete
discrete_mat = matrix(sample(1:5, 100, replace = TRUE), 10, 10)

# Create visualization
# Continuous variables
Heatmap(data_TCGA , name = "Methylation")
```

## Key Parameters
- `width`: Controls element width
- `position`: Position adjustment (identity, dodge, stack, fill)
- `fill`: Maps a variable to fill color for group comparison
- `color`: Maps a variable to outline/point color

## Tips
- The tutorial includes a '2. Customization' section with advanced styling options
- Adjust text size with `theme(text = element_text(size = 14))` for presentations
- Always check and report the correlation coefficient and p-value alongside visual patterns

## Full Tutorial
https://openbiox.github.io/Bizard/Correlation/ComplexHeatmap.html
