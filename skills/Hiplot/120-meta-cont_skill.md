# Skill: Meta-analysis of Continuous Data (R)

## Category
Hiplot

## When to Use
Create a Meta-analysis of Continuous Data using R with the Hiplot platform's approach. Suitable for biomedical data visualization with publication-quality output.

## Required R Packages
- data.table
- ggplotify
- jsonlite
- meta

## Minimal Reproducible Code
```r
# Load packages
library(data.table)
library(ggplotify)
library(jsonlite)
library(meta)

# Prepare data
# Load data
data <- data.table::fread(jsonlite::read_json("https://hiplot.cn/ui/basic/meta-cont/data.json")$exampleData$textarea[[1]])
data <- as.data.frame(data)

# Convert data structure
m1 <- metacont(n.e, mean.e, sd.e, n.c, mean.c, sd.c, studlab = Study, data = data,
              sm = "SMD")

# View data
head(data)

# Create visualization
# Meta-analysis of Continuous Data
p <- as.ggplot(function(){
  meta::forest(m1, layout = "meta")
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
https://openbiox.github.io/Bizard/Hiplot/120-meta-cont.html
