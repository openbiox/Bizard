# Skill: Correlogram (R)

## Category
Correlation

## When to Use
Correlogram or Correlation diagrams are often used to summarize the correlation information of various groups of data in the entire dataset.

## Required R Packages
- GGally
- corrgram
- corrplot
- ggcorrplot

## Minimal Reproducible Code
```r
# Load packages
library(GGally)
library(corrgram)
library(corrplot)
library(ggcorrplot)

# Prepare data
data("flea", package = "GGally")
data_flea <- flea

data("mtcars", package = "datasets")
data_mtcars <- mtcars

data("tips", package = "GGally")
data_tips <- tips
```

## Key Parameters
- `colour`: Maps `species` to the colour aesthetic
- `alpha`: Maps `0` to the alpha aesthetic
- `stat`: Statistical transformation to use

## Tips
- Adjust text size with `theme(text = element_text(size = 14))` for presentations
- Always check and report the correlation coefficient and p-value alongside visual patterns
- See the full tutorial for additional customization options and advanced examples

## Full Tutorial
https://openbiox.github.io/Bizard/Correlation/Correlogram.html
