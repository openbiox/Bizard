# Skill: Group Rank Dotplot (R)

## Category
Hiplot

## When to Use
Values distribution for different groups.

## Required R Packages
- data.table
- ggplot2
- jsonlite
- sigminer

## Minimal Reproducible Code
```r
# Load packages
library(data.table)
library(ggplot2)
library(jsonlite)
library(sigminer)

# Prepare data
# Load data
data <- data.table::fread(jsonlite::read_json("https://hiplot.cn/ui/basic/grdotplot/data.json")$exampleData$textarea[[1]])
data <- as.data.frame(data)

# View data
head(data)

# Create visualization
# Group Rank Dotplot
p <- show_group_distribution(data, gvar = "gvar",  dvar = "dvar", 
                             order_by_fun = F)

p
```

## Key Parameters
- `fill`: Maps a variable to fill color for group comparison
- `color`: Maps a variable to outline/point color

## Tips
- Adjust text size with `theme(text = element_text(size = 14))` for presentations
- See the full tutorial for additional customization options and advanced examples

## Full Tutorial
https://openbiox.github.io/Bizard/Hiplot/080-grdotplot.html
