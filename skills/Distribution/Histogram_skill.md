# Skill: Histogram (R)

## Category
Distribution

## When to Use
A histogram uses rectangular bars to represent the frequency of data within specific intervals, where the total area of the bars corresponds to the total frequency. It is primarily used to visualize the distribution of continuous variables.

## Required R Packages
- cowplot
- ggExtra
- ggplot2
- ggpmisc
- ggpubr
- readr
- tidyverse
- viridis

## Minimal Reproducible Code
```r
# Basic Histogram
p1 <- ggplot(data1, aes(x = expression)) +
  geom_histogram() + 
  labs(x = "Gene Expression", y = "Count")

p1
```

## Full Tutorial
https://openbiox.github.io/Bizard/Distribution/Histogram.html
