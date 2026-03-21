# Skill: 3D Pie (R)

## Category
Hiplot

## When to Use
The 3D pie chart is a pie chart that has a 3D appearance.

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
data <- data.table::fread(jsonlite::read_json("https://hiplot.cn/ui/basic/pie-3d/data.json")$exampleData$textarea[[1]])
data <- as.data.frame(data)

# Convert data structure
colnames(data) <- c("Group", "Value")
data$Value <- as.numeric(data$Value)
data <- data[data$Value != 0,]

# View data
head(data)

# Create visualization
# 3D Pie
pie3D(data$Value, radius = 0.8, height = 0.05, theta = 0.8,
      labels = paste(data$Group, "\n(n=", data$Value, ", ",
                     round(data$Value / sum(data$Value) * 100, 2), "%)",
                     sep = ""),
      explode = 0.1, main = "", labelcex = 1, shade = 0.4, labelcol = "black",
      col = c("#E64B35FF","#4DBBD5FF","#00A087FF","#3C5488FF"))
```

## Key Parameters
- `fill`: Maps a variable to fill color for group comparison
- `color`: Maps a variable to outline/point color

## Tips
- Adjust text size with `theme(text = element_text(size = 14))` for presentations
- See the full tutorial for additional customization options and advanced examples

## Full Tutorial
https://openbiox.github.io/Bizard/Hiplot/138-pie-3d.html
