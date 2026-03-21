# Skill: Funnel Plot (R)

## Category
Hiplot

## When to Use
Can be used to show potential bias factors in Meta-analysis.

## Required R Packages
- FunnelPlotR
- data.table
- gridExtra
- jsonlite

## Minimal Reproducible Code
```r
# Load packages
library(FunnelPlotR)
library(data.table)
library(gridExtra)
library(jsonlite)

# Prepare data
# Load data
data <- data.table::fread(jsonlite::read_json("https://hiplot.cn/ui/basic/funnel-plot/data.json")$exampleData$textarea[[1]])
data <- as.data.frame(data)

# View data
head(data)

# Create visualization
# Funnel Plot
p <- funnel_plot(
  data, numerator = los, denominator = prds,  group = provnum, data_type = "SR",
  limit = 99, label = "outlier", sr_method = "SHMI", trim_by=0.1, 
  title = "Funnel Plot", x_range = "auto", y_range = "auto"
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
https://openbiox.github.io/Bizard/Hiplot/058-funnel-plot.html
