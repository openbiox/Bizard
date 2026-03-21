# Skill: EnhancedMA (R)

## Category
Hiplot

## When to Use
Visualization of differentially expressed genes.

## Required R Packages
- EnhancedVolcano
- data.table
- jsonlite

## Minimal Reproducible Code
```r
# Load packages
library(EnhancedVolcano)
library(data.table)
library(jsonlite)

# Prepare data
# Load data
data <- data.table::fread(jsonlite::read_json("https://hiplot.cn/ui/basic/pseudo-enhanced-ma/data.json")$exampleData$textarea[[1]])
data <- as.data.frame(data)

# Convert data structure
row.names(data) <- data[,1]
data <- data[,-1]
data$baseMeanNew <- 1 / (10^log(data$baseMean + 1))

# View data
head(data)

# Create visualization
# EnhancedMA
p <- EnhancedVolcano(
  data, lab = rownames(data), title = "MA plot", subtitle = "EnhancedMA",
  x = 'log2FoldChange', y = 'baseMeanNew', xlab = bquote(~Log[2]~ 'fold change'),
  ylab = bquote(~Log[e]~ 'base mean + 1'), ylim = c(0,12),
  pCutoff = as.numeric(1e-05), FCcutoff = 1, pointSize = 3.5,
  labSize = 4, boxedLabels = T, colAlpha = 1,
  legendLabels = c('NS', expression(Log[2]~FC),
                   'Mean expression', 
                   expression(Mean-expression~and~log[2]~FC)),
  legendPosition = "bottom", legendLabSize = 16, legendIconSize = 4.0,
  encircleCol = 'black', encircleSize = 2.5, encircleFill = 'pink',
  encircleAlpha = 1/2) + 
  coord_flip() +
  theme(text = element_text(family = "Arial"),
        plot.title = element_text(size = 12,hjust = 0.5),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 10),
        axis.text.x = element_text(angle = 0, hjust = 0.5,vjust = 1),
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 10))

p
```

## Key Parameters
- `fill`: Maps a variable to fill color for group comparison
- `color`: Maps a variable to outline/point color

## Tips
- Use `coord_flip()` for horizontal orientation when labels are long
- Adjust text size with `theme(text = element_text(size = 14))` for presentations
- See the full tutorial for additional customization options and advanced examples

## Full Tutorial
https://openbiox.github.io/Bizard/Hiplot/143-pseudo-enhanced-ma.html
