# Skill: Ridgeline Plot (R)

## Category
Distribution

## When to Use
A ridgeline plot, also known as a joyplot, visualizes the distribution of multiple numeric variables across different categories. This method is useful for comparing density distributions while preserving an overall view of trends and variations.

## Required R Packages
- dplyr
- ggplot2
- ggridges
- hrbrthemes
- readr
- viridis

## Minimal Reproducible Code
```r
# Load packages
library(dplyr)
library(ggplot2)
library(ggridges)
library(hrbrthemes)
library(readr)
library(viridis)

# Prepare data
# Load iris dataset
data("iris")

# Load Lung Cancer (Raponi 2006) clinical data
TCGA_clinic <- readr::read_tsv("https://ucsc-public-main-xena-hub.s3.us-east-1.amazonaws.com/download/raponi2006_public%2Fraponi2006_public_clinicalMatrix.gz") %>%
  mutate(T = as.factor(T))
head(TCGA_clinic)

# Create visualization
# Basic Ridgeline plot
p1_1 <- ggplot(iris, aes(x = Sepal.Length, y = Species, fill = Species)) +
  geom_density_ridges(alpha = 0.5) +
  theme_ridges(font_size = 16, grid = TRUE) +
  theme(legend.position = "right")

p1_1
```

## Key Parameters
- `x`: Maps `OS` to the x aesthetic
- `y`: Maps `T` to the y aesthetic
- `fill`: Maps `T` to the fill aesthetic
- `alpha`: Controls transparency (0 = fully transparent, 1 = opaque)
- `position`: Position adjustment (identity, dodge, stack, fill)
- `stat`: Statistical transformation to use
- `theme`: Plot theme; tutorial uses `theme_ipsum()`

## Tips
- Customize color scales with `scale_fill_manual()` or `scale_color_brewer()`
- Adjust text size with `theme(text = element_text(size = 14))` for presentations
- Consider adding `geom_jitter()` or raw data points alongside distribution plots for small sample sizes

## Full Tutorial
https://openbiox.github.io/Bizard/Distribution/Ridgeline.html
