# Skill: Ternary chart (R)

## Category
Correlation

## When to use
A ternary chart is a type of chart used to display the proportional relationship between three variables. These three variables typically represent a certain component (such as chemical composition, species ratio, nutritional structure, etc.), and their sum is a constant, with the most common being 1 or 100%. A ternary chart uses an equilateral triangle to represent the proportional relationship between these three variables, with each point's position reflecting the relative proportion of th...

## Required R packages
- ggtern
- ggthemes

## Minimal reproducible code
```r
# Basic Ternary Chart
p1_1 <- ggtern(data=data, aes(x=CK, y=NPK, z=NPKM)) +
  geom_mask() +
  geom_point(aes(size=size,color=Genus),alpha=0.8)
p1_1
```

## Full tutorial
https://openbiox.github.io/Bizard/Correlation/TernaryPlot.html
