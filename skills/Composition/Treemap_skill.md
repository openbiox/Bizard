# Skill: Treemap (R)

## Category
Composition

## When to Use
A treemap, also known as a rectangular tree structure diagram, is composed of multiple nested rectangles of varying areas. The sum of the areas of all rectangles represents the overall data. The area of each smaller rectangle represents the proportion of each sub-item; the larger the rectangle's area, the larger the proportion of that sub-item within the whole.

## Required R Packages
- DOSE
- palmerpenguins
- tidyverse
- treemap

## Minimal Reproducible Code
```r
# Load packages
library(DOSE)
library(palmerpenguins)
library(tidyverse)
library(treemap)

# Prepare data
data_USArrests <- rownames_to_column(USArrests[1:8,], "State")

data_swiss <- swiss

data_countsub <- aggregate(penguins, by=list(penguins$species, penguins$sex),length)
data_countsub <- data_countsub[ ,1:3]
colnames(data_countsub) <- c("species", "sex", "count")

data_BP <- readr::read_csv("https://bizard-1301043367.cos.ap-guangzhou.myqcloud.com/data_BP.csv")
data_BP <- data_BP[order(abs(data_BP$NES), decreasing = T),]
data_BP <- data_BP[1:13,]

data_KEGG_type <- readr::read_csv("https://bizard-1301043367.cos.ap-guangzhou.myqcloud.com/data_KEGG.csv")
data_KEGG_type$pvalue_log <- -log10(data_KEGG_type$pvalue)

# Create visualization
treemap(data_USArrests, # data
        index = "State", # Categorical variables
        vSize = "Murder", # Categorical variable corresponding data values
        vColor="State", # The corresponding columns of color depth, here the data size is used as the corresponding
        type = "index", # Color mapping method, including "index", "value", "comp", "dens", "depth", "categorical", "color", and "manual".
        title = 'Murder', # title
        border.col = "grey", # Border color
        border.lwds = 4, # Border line width
        fontsize.labels = 12, # Label size
        fontcolor.labels = 'red', # Label color
        align.labels = list(c("center", "center")), # Tag location
        fontface.labels = 2) # Tag fonts: 1, 2, 3, 4 represent normal, bold, italic, and bold italic fonts, respectively.
```

## Key Parameters
- `width`: Controls element width
- `fill`: Maps a variable to fill color for group comparison
- `color`: Maps a variable to outline/point color

## Tips
- Adjust text size with `theme(text = element_text(size = 14))` for presentations
- Ensure proportions sum to 100% and consider using a colorblind-friendly palette
- See the full tutorial for additional customization options and advanced examples

## Full Tutorial
https://openbiox.github.io/Bizard/Composition/Treemap.html
