# Skill: D3 Wordcloud (R)

## Category
Hiplot

## When to Use
Display the wordcloud。

## Required R Packages
- d3wordcloud
- data.table
- jsonlite

## Minimal Reproducible Code
```r
# Load packages
library(d3wordcloud)
library(data.table)
library(jsonlite)

# Prepare data
# Load data
data <- data.table::fread(jsonlite::read_json("https://hiplot.cn/ui/basic/d3-wordcloud/data.json")$exampleData$textarea[[1]])
data <- as.data.frame(data)

# convert data structure
row.names(data) <- data[, 1]

# View data
head(data)

# Create visualization
# D3 Wordcloud
p <- d3wordcloud(
  words = data[, 1], 
  freqs = data[, 2],
  padding = 0,
  rotate.min = 0,
  rotate.max = 0,
  size.scale = "linear",
  color.scale = "linear",
  spiral = "archimedean",
  font = "Arial",
  rangesizefont = c(10, 90)
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
https://openbiox.github.io/Bizard/Hiplot/036-d3-wordcloud.html
