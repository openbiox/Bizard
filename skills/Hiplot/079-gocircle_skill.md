# Skill: GOCircle Plot (R)

## Category
Hiplot

## When to Use
The gocircle plot is used to display the circular plot combines gene expression and gene- annotation enrichment data. A subset of terms is displayed like the GOBar plot in combination with a scatter plot of the gene expression data. The whole plot is drawn on a specific coordinate system to achieve the circular layout. The segments are labeled with the term ID.

## Required R Packages
- GOplot
- data.table
- ggplotify
- jsonlite

## Minimal Reproducible Code
```r
# Load packages
library(GOplot)
library(data.table)
library(ggplotify)
library(jsonlite)

# Prepare data
# Load data
data <- data.table::fread(jsonlite::read_json("https://hiplot.cn/ui/basic/gocircle/data.json")$exampleData$textarea[[1]])
data <- as.data.frame(data)

# Convert data structure
colnames(data) <- c("category","ID","term","count","genes","logFC","adj_pval","zscore")
data <- data[!is.na(data$adj_pval),]
data$adj_pval <- as.numeric(data$adj_pval)
data$zscore <- as.numeric(data$zscore)
data$count <- as.numeric(data$count)

# View data
head(data)

# Create visualization
# GOCircle Plot
p <- function () {
  GOCircle(data, title = "GO Enrichment Circleplot",
           nsub = 10, rad1 = 2, rad2 = 3, table.legend = T, label.size = 5,
           zsc.col = c("#FC8D59","#FFFFBF","#99D594")) + 
    theme(plot.title = element_text(hjust = 0.5))
}
p <- as.ggplot(p)

p
```

## Key Parameters
- `fill`: Maps a variable to fill color for group comparison
- `color`: Maps a variable to outline/point color

## Tips
- Adjust text size with `theme(text = element_text(size = 14))` for presentations
- See the full tutorial for additional customization options and advanced examples

## Full Tutorial
https://openbiox.github.io/Bizard/Hiplot/079-gocircle.html
