# Skill: Circular Barplot (R)

## Category
Ranking

## When to use
Circular Barplot is a variation of the well-known bar chart where bars are displayed along a circle instead of a straight line. Note that while visually appealing, circular bar charts must be used with caution because the groups do not share the same Y-axis. However, they are well-suited for periodic data.

## Required R packages
- tidyverse

## Minimal reproducible code
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

## Full tutorial
https://openbiox.github.io/Bizard/Ranking/CircularBarplot.html
