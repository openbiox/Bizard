# Skill: PCAtools (R)

## Category
Hiplot

## When to Use
PCAtools can reduce the dimensionality of data through principal component analysis, and view principal component related features at a two-dimensional level

## Required R Packages
- PCAtools
- cowplot
- data.table
- ggplotify
- jsonlite

## Minimal Reproducible Code
```r
# Load packages
library(PCAtools)
library(cowplot)
library(data.table)
library(ggplotify)
library(jsonlite)

# Prepare data
# Load data
data <- data.table::fread(jsonlite::read_json("https://hiplot.cn/ui/basic/pcatools/data.json")$exampleData$textarea[[1]])
data <- as.data.frame(data)
data2 <- data.table::fread(jsonlite::read_json("https://hiplot.cn/ui/basic/pcatools/data.json")$exampleData$textarea[[2]])
data2 <- as.data.frame(data2)

# View data
head(data[,1:5])
head(data2[,1:5])

# Create visualization
# PCAtools
## Define the plot function
call_pcatools <- function(datTable, sampleInfo,
                          top_var,
                          screeplotComponents, screeplotColBar,
                          pairsplotComponents,
                          biplotShapeBy, biplotColBy,
                          plotloadingsComponents,
                          plotloadingsLowCol,
                          plotloadingsMidCol,
                          plotloadingsHighCol,
                          eigencorplotMetavars,
                          eigencorplotComponents) {
  row.names(datTable) <- datTable[, 1]
  datTable <- datTable[, -1]
  row.names(sampleInfo) <- sampleInfo[, 1]
  data3 <<- pca(datTable, metadata = sampleInfo, removeVar = (100 - top_var) / 100)

  for (i in c("screeplotComponents", "pairsplotComponents",
              "plotloadingsComponents", "eigencorplotComponents")) {
    if (ncol(data3$rotated) < get(i)) {
      assign(i, ncol(data3$rotated))
    }
  }

  p1 <- PCAtools::screeplot(
    data3,
    components = getComponents(data3, 1:screeplotComponents),
    axisLabSize = 14, titleLabSize = 20,
    colBar = screeplotColBar,
    gridlines.major = FALSE, gridlines.minor = FALSE,
# ... (see full tutorial for more)
```

## Key Parameters
- `width`: Controls element width
- `fill`: Maps a variable to fill color for group comparison
- `color`: Maps a variable to outline/point color

## Tips
- Adjust text size with `theme(text = element_text(size = 14))` for presentations
- See the full tutorial for additional customization options and advanced examples

## Full Tutorial
https://openbiox.github.io/Bizard/Hiplot/136-pcatools.html
