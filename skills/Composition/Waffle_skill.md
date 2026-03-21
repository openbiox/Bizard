# Skill: Waffle Chart (R)

## Category
Composition

## When to Use
A waffle chart visually represents categorical data using a grid of small squares that resemble waffles. Each category is assigned a unique color, and the number of squares assigned to each category corresponds to its proportion in the total data count.

## Required R Packages
- dplyr
- ggplot2
- waffle

## Minimal Reproducible Code
```r
# Load packages
library(dplyr)
library(ggplot2)
library(waffle)

# Prepare data
# 1.TCGA database (clinical data on lung cancer in 2020)
TCGA_cli_df <- readr::read_tsv("https://bizard-1301043367.cos.ap-guangzhou.myqcloud.com/raponi2006_public_raponi2006_public_clinicalMatrix.gz")
# Data Preparation
counts <- table(TCGA_cli_df$T)
counts <- as.data.frame(counts)
names(counts)[names(counts) == "Var1"] <- "T"

# 2.R built-in data - mtcars
counts1 <- table(mtcars$cyl)
counts1 <- as.data.frame(counts1)
names(counts1)[names(counts1) == "Var1"] <- "cyl"

# 3.Self-created dataset
data <- data.frame(
  group = c("First group", "First group", "First group", "First group",
            "First group", "First group", "Second group", "Second group",
            "Second group", "Second group", "Third group", "Third group"),
  subgroup = c("A", "B", "C", "D", "E", "F", "A", "B", "C", "D", "A", "B"),
  value = c(10, 20, 30, 40, 50, 60, 70, 80, 90, 100, 110, 120)
)

# Create visualization
waffle(counts)
```

## Key Parameters
- `fill`: Maps `subgroup` to the fill aesthetic
- `theme`: Plot theme; tutorial uses `theme_void()`
- `color`: Maps a variable to outline/point color

## Tips
- Customize color scales with `scale_fill_manual()` or `scale_color_brewer()`
- Use `facet_wrap()` or `facet_grid()` to create multi-panel plots by group
- Ensure proportions sum to 100% and consider using a colorblind-friendly palette

## Full Tutorial
https://openbiox.github.io/Bizard/Composition/Waffle.html
