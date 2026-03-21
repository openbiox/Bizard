# Skill: Scatterstats (R)

## Category
Hiplot

## When to Use
Create a Scatterstats using R with the Hiplot platform's approach. Suitable for biomedical data visualization with publication-quality output.

## Required R Packages
- data.table
- ggstatsplot
- jsonlite

## Minimal Reproducible Code
```r
# Load packages
library(data.table)
library(ggstatsplot)
library(jsonlite)

# Prepare data
# Load data
data <- data.table::fread(jsonlite::read_json("https://hiplot.cn/ui/basic/ggscatterstats/data.json")$exampleData$textarea[[1]])
data <- as.data.frame(data)

# View data
head(data)

# Create visualization
# Scatterstats
p <- ggscatterstats(
  data = data, x = rating, y = budget
)

p
```

## Key Parameters
- `stat`: Statistical transformation to use
- `fill`: Maps a variable to fill color for group comparison
- `color`: Maps a variable to outline/point color

## Tips
- Adjust text size with `theme(text = element_text(size = 14))` for presentations
- See the full tutorial for additional customization options and advanced examples

## Full Tutorial
https://openbiox.github.io/Bizard/Hiplot/073-ggscatterstats.html
