# Skill: Cox Models Forest (R)

## Category
Hiplot

## When to Use
Cox model forest is a visual representation of a COX model that constructs a risk forest map to facilitate variable screening.

## Required R Packages
- data.table
- ezcox
- jsonlite

## Minimal Reproducible Code
```r
# Load packages
library(data.table)
library(ezcox)
library(jsonlite)

# Prepare data
# Load data
data <- data.table::fread(jsonlite::read_json("https://hiplot.cn/ui/basic/ezcox/data.json")$exampleData$textarea[[1]])
data <- as.data.frame(data)

# View data
head(data)

# Create visualization
# Cox Models Forest
p <- show_forest(
  data = data,
  covariates = c("sex", "ph.ecog"),
  controls = "age",
  merge_models = F,
  drop_controls = F,
  add_caption = T
)

p
```

## Key Parameters
- `fill`: Maps a variable to fill color for group comparison
- `color`: Maps a variable to outline/point color

## Tips
- Adjust text size with `theme(text = element_text(size = 14))` for presentations
- See the full tutorial for additional customization options and advanced examples

## Full Tutorial
https://openbiox.github.io/Bizard/Hiplot/053-ezcox.html
