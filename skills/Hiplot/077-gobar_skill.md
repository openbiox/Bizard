# Skill: GOBar Plot (R)

## Category
Hiplot

## When to Use
The gobar plot is used to display Z-score coloured barplot of terms ordered alternatively by z-score or the negative logarithm of the adjusted p-value.

## Required R Packages
- GOplot
- data.table
- jsonlite

## Minimal Reproducible Code
```r
# Load packages
library(GOplot)
library(data.table)
library(jsonlite)

# Prepare data
# Load data
data <- data.table::fread(jsonlite::read_json("https://hiplot.cn/ui/basic/gobar/data.json")$exampleData$textarea[[1]])
data <- as.data.frame(data)

# Convert data structure
colnames(data) <- c("category","ID","term","count","genes","logFC","adj_pval","zscore")
data <- data[data$category %in% c("BP","CC","MF"),]
data <- data[!is.na(data$adj_pval),]
data$adj_pval <- as.numeric(data$adj_pval)
data$zscore <- as.numeric(data$zscore)

# View data
head(data)

# Create visualization
# GOBar Plot
p <- GOBar(data, display = "multiple", order.by.zscore = T,
           title = "GO Enrichment Barplot ", 
           zsc.col = c("#EF8A62","#F7F7F7","#67A9CF")) + 
    theme(plot.title = element_text(hjust = 0.5),
          axis.text.x = element_text(size = 8))

p
```

## Key Parameters
- `fill`: Maps a variable to fill color for group comparison
- `color`: Maps a variable to outline/point color

## Tips
- Adjust text size with `theme(text = element_text(size = 14))` for presentations
- See the full tutorial for additional customization options and advanced examples

## Full Tutorial
https://openbiox.github.io/Bizard/Hiplot/077-gobar.html
