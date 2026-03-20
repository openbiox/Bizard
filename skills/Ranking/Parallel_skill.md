# Skill: Parallel Coordinates Plot (R)

## Category
Ranking

## When to use
Parallel coordinate plots are a common method for visualizing high-dimensional multivariate data. To display a set of objects in a multidimensional space, multiple parallel and equally spaced axes are drawn, and the objects in the multidimensional space are represented as broken lines with vertices on the parallel axes. Although parallel line plots are a special type of line plot, they differ significantly from ordinary line plots. This is because parallel line plots are not limited to descri...

## Required R packages
- GGally
- MASS
- RColorBrewer
- dplyr
- ggbump
- hrbrthemes
- patchwork
- tibble
- tidyr
- viridis

## Minimal reproducible code
```r
# Basic parallel graph
p <- ggparcoord(data_iris, columns = 1:4, groupColumn = 5) 

p
```

## Full tutorial
https://openbiox.github.io/Bizard/Ranking/Parallel.html
