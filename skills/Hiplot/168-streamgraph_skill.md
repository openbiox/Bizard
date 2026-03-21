# Skill: Streamgraph (R)

## Category
Hiplot

## When to Use
Create a Streamgraph using R with the Hiplot platform's approach. Suitable for biomedical data visualization with publication-quality output.

## Required R Packages
- data.table
- jsonlite
- streamgraph

## Minimal Reproducible Code
```r
# Load packages
library(data.table)
library(jsonlite)
library(streamgraph)

# Prepare data
# Load data
data <- data.table::fread(jsonlite::read_json("https://hiplot.cn/ui/basic/streamgraph/data.json")$exampleData$textarea[[1]])
data <- as.data.frame(data)

# convert data structure
colnames(data) <- c("date","key","value")

# View data
head(data)

# Create visualization
# Streamgraph
p <- streamgraph(data, key = "key", value = "value", date = "date",
                 offset = "silhouette", interpolate = "cardinal",
                 interactive = F, scale = "date") %>% 
  sg_fill_brewer(palette = "Spectral")

p
```

## Key Parameters
- `fill`: Maps a variable to fill color for group comparison
- `color`: Maps a variable to outline/point color

## Tips
- Adjust text size with `theme(text = element_text(size = 14))` for presentations
- See the full tutorial for additional customization options and advanced examples

## Full Tutorial
https://openbiox.github.io/Bizard/Hiplot/168-streamgraph.html
