# Skill: Beanplot (R)

## Category
Hiplot

## When to Use
The beanplot is a method of visualizing the distribution characteristics.

## Required R Packages
- beanplot
- data.table
- ggplotify
- jsonlite

## Minimal Reproducible Code
```r
# Load packages
library(beanplot)
library(data.table)
library(ggplotify)
library(jsonlite)

# Prepare data
# Load data
data <- data.table::fread(jsonlite::read_json("https://hiplot.cn/ui/basic/beanplot/data.json")$exampleData$textarea[[1]])
data <- as.data.frame(data)

# convert data structure
GroupOrder <- as.numeric(factor(data[, 2], levels = unique(data[, 2])))
data[, 2] <- paste0(data[,2], " ", as.numeric(factor(data[, 3])))
data <- cbind(data, GroupOrder)

# View data
head(data)

# Create visualization
# Beanplot
p <- as.ggplot(function() {
  beanplot(Y ~ reorder(X, GroupOrder, mean), data = data, ll = 0.04,
           main = "Bean Plot", ylab = "Y", xlab = "X", side = "both",
           border = NA, horizontal = F, 
           col = list(c("#2b70c4", "#2b70c4"),c("#e9c216", "#e9c216")),
           beanlines = "mean", overallline = "mean", kernel = "gaussian")
  
  legend("bottomright", fill = c("#2b70c4", "#e9c216"),
         legend = levels(factor(data[, 3])))
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
https://openbiox.github.io/Bizard/Hiplot/011-beanplot.html
