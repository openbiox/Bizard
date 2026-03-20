# Skill: Histogram (R)

## Category
Distribution

## When to use
A histogram uses rectangular bars to represent the frequency of data within specific intervals, where the total area of the bars corresponds to the total frequency. It is primarily used to visualize the distribution of continuous variables.

## Required R packages
- cowplot
- ggExtra
- ggplot2
- ggpmisc
- ggpubr
- readr
- tidyverse
- viridis

## Minimal reproducible code
```r
# Basic Histogram
p1 <- ggplot(data1, aes(x = expression)) +
  geom_histogram() + 
  labs(x = "Gene Expression", y = "Count")

p1
```

## Full tutorial
https://openbiox.github.io/Bizard/Distribution/Histogram.html
