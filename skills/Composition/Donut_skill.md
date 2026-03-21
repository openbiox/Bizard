# Skill: Donut Chart (R)

## Category
Composition

## When to Use
A donut chart is a circular plot divided into sectors, each sector representing a part of the whole. It is very similar to a pie chart and can be constructed in ggplot2 and basic R.

## Required R Packages
- ggplot2

## Minimal Reproducible Code
```r
# Load packages
library(ggplot2)

# Prepare data
# 1.TCGA database (clinical data on lung cancer in 2020)
TCGA_cli_df <- readr::read_tsv("https://bizard-1301043367.cos.ap-guangzhou.myqcloud.com/raponi2006_public_raponi2006_public_clinicalMatrix.gz")

# 2.R built-in data - mtcars
head(mtcars)

# Create visualization
# Data Preparation
counts <- table(TCGA_cli_df$T)
counts <- as.data.frame(counts)
names(counts)[names(counts) == "Var1"] <- "T"
# Calculate percentage
counts$fraction = counts$Freq / sum(counts$Freq)
# Calculate the cumulative percentage (the value at the top of each rectangle).
counts$ymax = cumsum(counts$fraction)
# Calculate the bottom of each rectangle to determine the starting position
counts$ymin = c(0, head(counts$ymax, n=-1))
# Plot
p <- ggplot(counts, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=T)) +
  geom_rect() +
  coord_polar(theta="y") + 
  xlim(c(2, 4)) 

p
```

## Key Parameters
- `fill`: Maps `cyl` to the fill aesthetic
- `y`: Maps `labelPosition` to the y aesthetic
- `color`: Maps `cyl` to the color aesthetic
- `position`: Position adjustment (identity, dodge, stack, fill)
- `theme`: Plot theme; tutorial uses `theme_void()`

## Tips
- Customize color scales with `scale_fill_manual()` or `scale_color_brewer()`
- Adjust text size with `theme(text = element_text(size = 14))` for presentations
- Ensure proportions sum to 100% and consider using a colorblind-friendly palette

## Full Tutorial
https://openbiox.github.io/Bizard/Composition/Donut.html
