# Skill: Perspective (R)

## Category
Hiplot

## When to Use
The three-dimensional perspective is a three-dimensional figure that can connect the higher values contained in a matrix with surfaces.

## Required R Packages
- data.table
- ggplotify
- jsonlite
- shape

## Minimal Reproducible Code
```r
# Load packages
library(data.table)
library(ggplotify)
library(jsonlite)
library(shape)

# Prepare data
# Load data
data <- data.table::fread(jsonlite::read_json("https://hiplot.cn/ui/basic/perspective/data.json")$exampleData$textarea[[1]])
data <- as.data.frame(data)

# Convert data structure
data <- as.matrix(data)
col <- drapecol(data)

# View data
head(data[,1:5])

# Create visualization
# Perspective
p <- as.ggplot(function() {
  persp(as.matrix(data),
    theta = 45, phi = 20,
    expand = 0.5,
    r = 180, col = col,
    ltheta = 120,
    shade = 0.5,
    ticktype = "detailed",
    xlab = "X", ylab = "Y", zlab = "Z",
    border = "black" # could be NA
  )
  title("Perspective Plot", line = 0)
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
https://openbiox.github.io/Bizard/Hiplot/137-perspective.html
