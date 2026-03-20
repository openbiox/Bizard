# Skill: Grouped and Stacked Barplot (R)

## Category
Composition

## When to use
Grouped bar charts, or clustered bar charts, extend the functionality of univariate or single-category bar charts to multivariate bar charts. In these charts, bars are grouped according to their categories, and colors represent distinguishing factors for other categorical variables. The bars are positioned to cater to a group or primary group, with colors representing secondary categories. Grouped bar charts are particularly suitable for displaying the distribution of multiple groups of categ...

## Required R packages
- RColorBrewer
- dplyr
- ggplot2
- hrbrthemes
- streamgraph
- tibble
- tidyr
- viridis

## Minimal reproducible code
```r
# Grouped bar chart
p <- ggplot(iris_means_long, aes(fill = Species, y = Value, x = Measurement)) + 
  geom_bar(position = "dodge", stat = "identity") +
  scale_fill_manual(values = c("#4A8CC3", "#E37939", "#8FBC89")) + 
  labs(x = "Measurement", 
       y = "Mean Value") + 
  theme_minimal()

p
```

## Full tutorial
https://openbiox.github.io/Bizard/Composition/GroupedBarplot.html
