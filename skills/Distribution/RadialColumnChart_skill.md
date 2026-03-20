# Skill: Radial Column Chart (R)

## Category
Distribution

## When to use
Visualize radial column chart data in a biomedical context.

## Required R packages
- dplyr
- ggforce
- ggplot2
- scales

## Minimal reproducible code
```r
# Basic histogram
p1 <- ggplot(df, aes(x = factor(id), y = value)) +
  geom_col(aes(fill = group), width = 0.8, alpha = 0.8) +
  coord_radial(inner.radius = 0.3) +
  scale_fill_manual(values = c("#1E88E5", "#D81B60")) +
  theme_void() +
  labs(title = "Comparison of indicators between the treatment group and the control group")
p1
```

## Full tutorial
https://openbiox.github.io/Bizard/Distribution/RadialColumnChart.html
