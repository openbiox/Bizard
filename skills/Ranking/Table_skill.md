# Skill: Table (R)

## Category
Ranking

## When to Use
Tables are both a visual communication mode and a means of organizing and collating data.

## Required R Packages
- dplyr
- gt
- gtExtras
- readr

## Minimal Reproducible Code
```r
# Load packages
library(dplyr)
library(gt)
library(gtExtras)
library(readr)

# Prepare data
# 1.Select the first 7 rows of the iris dataset
data <- iris[1:7,]

head(data)

# 2.The first 7 rows of clinical data on gastric cancer from the UCSC Xena database (this clinical data was only used when creating the three-line table).
data_clinical <- read.table("https://bizard-1301043367.cos.ap-guangzhou.myqcloud.com/TCGA-STAD.survival.tsv", header = TRUE, sep = "\t")
data_clinical <- data_clinical[1:7,]

head(data_clinical)

# Create visualization
# You can draw the graph by calling the gt() function.
gt(data)
```

## Key Parameters
- `alpha`: Controls transparency (0 = fully transparent, 1 = opaque)
- `position`: Position adjustment (identity, dodge, stack, fill)
- `fill`: Maps a variable to fill color for group comparison
- `color`: Maps a variable to outline/point color

## Tips
- Adjust text size with `theme(text = element_text(size = 14))` for presentations
- Sort categories by value rather than alphabetically for clearer ranking visualization
- See the full tutorial for additional customization options and advanced examples

## Full Tutorial
https://openbiox.github.io/Bizard/Ranking/Table.html
