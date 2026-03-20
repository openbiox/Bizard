# Skill: Bar Plot (R)

## Category
Ranking

## When to use
A bar plot is a graph that uses the height or length of the bars to represent the amount of data.

## Required R packages
- cowplot
- dplyr
- forcats
- ggpattern
- ggplot2
- ggpubr
- hrbrthemes
- magrittr
- palmerpenguins
- rstatix
- tidyr

## Minimal reproducible code
```r
# Basic bar plot
p <- ggplot(data_tcga_mean, aes(x=gene, y=expression)) + 
  geom_bar(stat = "identity")

p
```

## Full tutorial
https://openbiox.github.io/Bizard/Ranking/BarPlot.html
