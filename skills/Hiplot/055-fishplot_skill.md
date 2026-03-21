# Skill: Fishplot (R)

## Category
Hiplot

## When to Use
Clone evolution analysis

## Required R Packages
- data.table
- fishplot
- jsonlite

## Minimal Reproducible Code
```r
# Load packages
library(data.table)
library(fishplot)
library(jsonlite)

# Prepare data
# Load data
data <- data.table::fread(jsonlite::read_json("https://hiplot.cn/ui/basic/fishplot/data.json")$exampleData$textarea[[1]])
data <- as.data.frame(data)

# View data
head(data)

# Create visualization
## Create a fish object
fish = createFishObject(as.matrix(data[,4:7]), parents=data$parents, 
                        timepoints=data$timepoints, 
                        col = c("#888888","#e8130c","#f8150d","#55158f"))
## Calculate the layout of the drawing
fish = layoutClones(fish)
## Draw the plot, using the splining method (recommended), and providing both timepoints to label and a plot title
fishPlot(fish,shape="spline", title.btm="Sample1", title = "Fishplot",
         cex.title=1, vlines=c(0,30,75,150), 
         vlab=c("Day 0","Day 30","Day 75","Day 150"))
```

## Key Parameters
- `fill`: Maps a variable to fill color for group comparison
- `color`: Maps a variable to outline/point color

## Tips
- Adjust text size with `theme(text = element_text(size = 14))` for presentations
- See the full tutorial for additional customization options and advanced examples

## Full Tutorial
https://openbiox.github.io/Bizard/Hiplot/055-fishplot.html
