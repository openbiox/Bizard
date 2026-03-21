# Skill: Pyramid Stack2 (R)

## Category
Hiplot

## When to Use
The pyramid stack is a pyramid-like figure that distributes data on both sides of a central axis.

## Required R Packages
- data.table
- ggplotify
- jsonlite
- plotrix

## Minimal Reproducible Code
```r
# Load packages
library(data.table)
library(ggplotify)
library(jsonlite)
library(plotrix)

# Prepare data
# Load data
data <- data.table::fread(jsonlite::read_json("https://hiplot.cn/ui/basic/pyramid-stack2/data.json")$exampleData$textarea[[1]])
data <- as.data.frame(data)

# Convert data structure
agegrps <- unique(data[,1])
split_var <- unique(data[,2])
dat_left <- as.matrix(data[data[,2] == split_var[1],-c(1,2)])
dat_right <- as.matrix(data[data[,2] == split_var[2],-c(1,2)])

# View data
head(data)

# Create visualization
# Pyramid Stack2
p <- as.ggplot(function() {
  cols <- c("#E64B35FF","#4DBBD5FF","#00A087FF","#3C5488FF")
  names(cols) <- colnames(dat_left)
  cols <- cols[1:ncol(dat_left)]
  pyramid.plot(dat_left, dat_right, labels = agegrps, unit = "Value",
               lxcol = cols, rxcol = cols,
               laxlab=as.numeric(c(0,10,20,30)), raxlab=as.numeric(c(0,10,20,30)),
               top.labels=c(split_var[1], colnames(data)[1], split_var[2]),
               gap=4, ppmar=c(4,2,4,7), do.first="plot_bg(\"#FFFFFF\")")
  mtext("Porridge temperature by age and sex of bear", 3, 2, cex=1)
  legend("right", inset=c(-0.25,0), legend = colnames(dat_left), fill = cols)
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
https://openbiox.github.io/Bizard/Hiplot/147-pyramid-stack2.html
