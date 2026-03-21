# Skill: Funnel Plot (metafor) (R)

## Category
Hiplot

## When to Use
Can be used to show potential bias factors in Meta-analysis.

## Required R Packages
- data.table
- ggplotify
- jsonlite
- metafor

## Minimal Reproducible Code
```r
# Load packages
library(data.table)
library(ggplotify)
library(jsonlite)
library(metafor)

# Prepare data
# Load data
data <- data.table::fread(jsonlite::read_json("https://hiplot.cn/ui/basic/funnel-plot-metafor/data.json")$exampleData$textarea[[1]])
data <- as.data.frame(data)

# Convert data structure
data2 <- escalc(ri=ri, ni=ni, data = data, measure="ZCOR")
res <- rma(yi, vi, data = data2)

# View data
head(data)

# Create visualization
# Funnel Plot
p <- as.ggplot(function(){
  funnel(x = res, main = "Funnel Plot (metafor)",
         level = c(90, 95, 99), shade = c("white","#a90e07","#d23e0b"), refline = 0)
  })

p
```

## Key Parameters
- `fill`: Maps a variable to fill color for group comparison
- `color`: Maps a variable to outline/point color

## Tips
- Adjust text size with `theme(text = element_text(size = 14))` for presentations
- See the full tutorial for additional customization options and advanced examples

## Full Tutorial
https://openbiox.github.io/Bizard/Hiplot/057-funnel-plot-metafor.html
