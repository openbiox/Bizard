# Skill: Violin Plot (R)

## Category
Distribution

## When to use
A violin plot combines elements of a density plot and a box plot to visualize data distribution. It displays key statistical information, including the median, quartiles, minimum, and maximum values. Violin plots are particularly useful for comparing distributions across different groups, offering a more intuitive representation than traditional box plots by revealing the shape of the data distribution.

## Required R packages
- dplyr
- forcats
- gghalves
- ggplot2
- ggpubr
- ggstatsplot
- hrbrthemes
- palmerpenguins
- readr
- tidyr
- viridis

## Minimal reproducible code
```r
# Basic Violin Plot
p <- ggplot(data, aes(x=name, y=value, fill=name)) + 
  geom_violin()

p
```

## Full tutorial
https://openbiox.github.io/Bizard/Distribution/ViolinPlot.html
