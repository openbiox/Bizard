# Skill: Likert Plot (R)

## Category
Hiplot

## When to Use
Descriptive statistical analysis of Likert scale data.

## Required R Packages
- data.table
- ggplotify
- jsonlite
- likert

## Minimal Reproducible Code
```r
# Load packages
library(data.table)
library(ggplotify)
library(jsonlite)
library(likert)

# Prepare data
# Load data
data <- data.table::fread(jsonlite::read_json("https://hiplot.cn/ui/basic/likert/data.json")$exampleData$textarea[[1]])
data <- as.data.frame(data)

# Convert data structure
levs <- unique(unlist(data))
for (i in 1:ncol(data)) {
  data[,i] <- factor(data[, i], levels = levs)
}

# View data
head(data)

# Create visualization
# Likert Plot
pobj <- likert(data)
colrs <- c("#3B4992FF","#EE0000FF")
p <- as.ggplot(plot(pobj, type = "bar", 
                    low.color = colrs[1], high.color = colrs[2], wrap = 50))

p
```

## Key Parameters
- `fill`: Maps a variable to fill color for group comparison
- `color`: Maps a variable to outline/point color

## Tips
- Adjust text size with `theme(text = element_text(size = 14))` for presentations
- See the full tutorial for additional customization options and advanced examples

## Full Tutorial
https://openbiox.github.io/Bizard/Hiplot/091-likert.html
