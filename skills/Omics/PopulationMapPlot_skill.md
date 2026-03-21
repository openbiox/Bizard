# Skill: Population Map Plot (R)

## Category
Omics

## When to Use
Create a Population Map Plot visualization in R for biomedical data analysis and research publications.

## Required R Packages
- doParallel
- dplyr
- ggfx
- ggnewscale
- ggplot2
- ggrepel
- ggspatial
- gstat
- rnaturalearth
- rnaturalearthdata
- sf
- viridis

## Minimal Reproducible Code
```r
# Load packages
library(doParallel)
library(dplyr)
library(ggfx)
library(ggnewscale)
library(ggplot2)
library(ggrepel)

# Prepare data
# Global geographic data
world <- ne_countries(scale = "medium", returnclass = "sf")
# Simulating epidemiological data
set.seed(123)
world$incidence <- runif(nrow(world), 0, 100)  # Randomly generate incidence data

# Create visualization
# Basic map of global disease incidence distribution
p1 <- ggplot(data = world) +
  geom_sf(aes(fill = incidence)) +
  scale_fill_viridis(option = "C") +
  labs(title = "Global Disease Incidence",
       fill = "Incidence Rate\n(per 100k)")

p1
```

## Key Parameters
- `fill`: Maps `incidence` to the fill aesthetic
- `color`: Maps `var1` to the color aesthetic
- `alpha`: Controls transparency (0 = fully transparent, 1 = opaque)
- `width`: Controls element width
- `position`: Position adjustment (identity, dodge, stack, fill)
- `stat`: Statistical transformation to use
- `theme`: Plot theme; tutorial uses `theme_void()`

## Tips
- The tutorial includes a '2. Advanced plot' section with advanced styling options
- Customize color scales with `scale_fill_manual()` or `scale_color_brewer()`
- Include appropriate statistical thresholds (e.g., FDR < 0.05, |log2FC| > 1) in the visualization

## Full Tutorial
https://openbiox.github.io/Bizard/Omics/PopulationMapPlot.html
