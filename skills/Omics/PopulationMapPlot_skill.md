# Skill: Population Map Plot (R)

## Category
Omics

## When to use
Visualize population map plot data in a biomedical context.

## Required R packages
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

## Minimal reproducible code
```r
# Basic map of global disease incidence distribution
p1 <- ggplot(data = world) +
  geom_sf(aes(fill = incidence)) +
  scale_fill_viridis(option = "C") +
  labs(title = "Global Disease Incidence",
       fill = "Incidence Rate\n(per 100k)")

p1
```

## Full tutorial
https://openbiox.github.io/Bizard/Omics/PopulationMapPlot.html
