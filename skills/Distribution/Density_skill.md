# Skill: Density Plot (R)

## Category
Distribution

## When to Use
A density plot represents the distribution of a numerical variable using kernel density estimation to display the probability density function. It is a smoothed version of a histogram, sharing the same concept but providing a clearer representation of the overall trend and shape of the data.

## Required R Packages
- cowplot
- dplyr
- geomtextpath
- ggExtra
- ggplot2
- ggpmisc
- ggpubr
- hrbrthemes
- readr
- tidyr
- viridis

## Minimal Reproducible Code
```r
# Basic Density Plot 
p1 <- ggplot(data1, aes(x = expression)) +   
  geom_density(fill = "#69b3a2", color = "#e9ecef", alpha = 0.8) +   
  labs(title = "Density Plot of TSPAN6 Expression Levels",        
       x = "Expression",        
       y = "Density") +   
  theme_minimal()  
p1
```

## Full Tutorial
https://openbiox.github.io/Bizard/Distribution/Density.html
