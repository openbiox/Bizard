# Skill: Meta-Subgroup Analysis (R)

## Category
Hiplot

## When to Use
The goal of metawho is to provide simple R implementation of “Meta-analytical method to Identify Who Benefits Most from Treatments”.

## Required R Packages
- cowplot
- data.table
- jsonlite
- metawho

## Minimal Reproducible Code
```r
# Load packages
library(cowplot)
library(data.table)
library(jsonlite)
library(metawho)

# Prepare data
# Load data
data <- data.table::fread(jsonlite::read_json("https://hiplot.cn/ui/basic/metawho/data.json")$exampleData$textarea[[1]])
data <- as.data.frame(data)

# Convert data structure
data = deft_prepare(data, conf_level = 1 - 0.95)
res = deft_do(data, group_level = unique(data$subgroup))

# View data
head(data)

# Create visualization
# Meta-Subgroup Analysis
p1 <- deft_show(res, element = "all")
p2 <- deft_show(res, element = "subgroup")
p <- plot_grid(p1, p2, nrow = 2)

p
```

## Key Parameters
- `fill`: Maps a variable to fill color for group comparison
- `color`: Maps a variable to outline/point color

## Tips
- Adjust text size with `theme(text = element_text(size = 14))` for presentations
- See the full tutorial for additional customization options and advanced examples

## Full Tutorial
https://openbiox.github.io/Bizard/Hiplot/121-metawho.html
