# Skill: Simple Funnel Diagram (R)

## Category
Hiplot

## When to Use
Create a Simple Funnel Diagram using R with the Hiplot platform's approach. Suitable for biomedical data visualization with publication-quality output.

## Required R Packages
- data.table
- echarts4r
- jsonlite
- magrittr

## Minimal Reproducible Code
```r
# Load packages
library(data.table)
library(echarts4r)
library(jsonlite)
library(magrittr)

# Prepare data
# Load data
data <- data.table::fread(jsonlite::read_json("https://hiplot.cn/ui/basic/simple-funnel-diagram/data.json")$exampleData$textarea[[1]])
data <- as.data.frame(data)

# View data
head(data)

# Create visualization
# Simple Funnel Diagram
p <- data %>%
  e_charts() %>%
  e_funnel(value, key) %>%
  e_title("Funnel") %>%
  e_theme("macarons")

p
```

## Key Parameters
- `fill`: Maps a variable to fill color for group comparison
- `color`: Maps a variable to outline/point color

## Tips
- Adjust text size with `theme(text = element_text(size = 14))` for presentations
- See the full tutorial for additional customization options and advanced examples

## Full Tutorial
https://openbiox.github.io/Bizard/Hiplot/164-simple-funnel-diagram.html
