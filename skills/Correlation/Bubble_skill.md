# Skill: Bubble Plot (R)

## Category
Correlation

## When to Use
A bubble plot is a scatter plot in which a third numeric variable is mapped to the size of the circles. This article shows several ways to build bubble charts using R.

## Required R Packages
- dplyr
- gapminder
- ggplot2
- hrbrthemes
- viridis

## Minimal Reproducible Code
```r
# Load packages
library(dplyr)
library(gapminder)
library(ggplot2)
library(hrbrthemes)
library(viridis)

# Prepare data
# R built-in data - iris
head(iris)

# TCGA database (using clinical data on lung cancer in 2020)
TCGA_clinic <- readr::read_tsv("https://bizard-1301043367.cos.ap-guangzhou.myqcloud.com/raponi2006_public_raponi2006_public_clinicalMatrix.gz")
TCGA_clinic$T <- as.factor(TCGA_clinic$T)

# gapminder package
data <- gapminder %>% 
  filter(year=="2007") %>% 
  dplyr::select(-year)

# Create visualization
# Taking iris data as an example
p <- ggplot(iris, aes(x=Sepal.Length, y=Sepal.Width, size = Species)) +
  geom_point(alpha=0.4)

p
```

## Key Parameters
- `x`: Maps `Sepal` to the x aesthetic
- `y`: Maps `Sepal` to the y aesthetic
- `size`: Maps `Species` to the size aesthetic
- `color`: Maps `Species` to the color aesthetic
- `alpha`: Controls transparency (0 = fully transparent, 1 = opaque)
- `position`: Position adjustment (identity, dodge, stack, fill)
- `theme`: Plot theme; tutorial uses `theme_ipsum()`

## Tips
- Customize color scales with `scale_fill_manual()` or `scale_color_brewer()`
- Adjust text size with `theme(text = element_text(size = 14))` for presentations
- Always check and report the correlation coefficient and p-value alongside visual patterns

## Full Tutorial
https://openbiox.github.io/Bizard/Correlation/Bubble.html
