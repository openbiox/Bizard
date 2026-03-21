# Skill: Circular Barplot (R)

## Category
Ranking

## When to Use
Circular Barplot is a variation of the well-known bar chart where bars are displayed along a circle instead of a straight line. Note that while visually appealing, circular bar charts must be used with caution because the groups do not share the same Y-axis. However, they are well-suited for periodic data.

## Required R Packages
- tidyverse

## Minimal Reproducible Code
```r
# Load packages
library(tidyverse)

# Prepare data
# 1.R's built-in data—iris
head(iris)

# 2.Self-built dataset
data_customize <- data.frame(
  individual=paste( "Mister ", seq(1,60), sep=""),
  group=c( rep('A', 10), rep('B', 30), rep('C', 14), rep('D', 6)) ,
  value=sample( seq(10,100), 60, replace=T)
)

# 3.TCGA database (gene expression data for liver cancer)
tcga_circle <- readr::read_csv(
"https://bizard-1301043367.cos.ap-guangzhou.myqcloud.com/tcga_circle.csv")

# Create visualization
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

## Key Parameters
- `x`: Maps `title` to the x aesthetic
- `y`: Maps `value` to the y aesthetic
- `fill`: Maps `group` to the fill aesthetic
- `alpha`: Controls transparency (0 = fully transparent, 1 = opaque)
- `width`: Controls element width
- `position`: Position adjustment (identity, dodge, stack, fill)
- `stat`: Statistical transformation to use
- `theme`: Plot theme; tutorial uses `theme_void()`

## Tips
- The tutorial includes a '3. Beautify plot' section with advanced styling options
- Use `theme_minimal()` or `theme_bw()` for clean, publication-ready plots
- Sort categories by value rather than alphabetically for clearer ranking visualization

## Full Tutorial
https://openbiox.github.io/Bizard/Ranking/CircularBarplot.html
