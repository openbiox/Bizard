# Skill: Circular Barplot (R)

## Category
Ranking

## When to Use
Circular Barplot is a variation of the well-known bar chart where bars are displayed along a circle instead of a straight line. Note that while visually appealing, circular bar charts must be used with caution because the groups do not share the same Y-axis. However, they are well-suited for periodic data.

## Required R Packages
- tidyverse

## Minimal Reproducible Code
```r
iris_id <- iris[order(iris$Species),]
iris_id$new_column <- 1:nrow(iris_id)
p <- ggplot(iris_id, aes(x = new_column, y = Sepal.Length, fill = Species)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(start = 0) +
  theme_void() + 
  labs(fill = "Species", y = "Sepal.Length", x = NULL) +
  theme(legend.title = element_blank()) 

p
```

## Full Tutorial
https://openbiox.github.io/Bizard/Ranking/CircularBarplot.html
