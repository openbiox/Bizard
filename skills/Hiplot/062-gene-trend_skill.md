# Skill: Gene Cluster Trend (R)

## Category
Hiplot

## When to Use
The gene cluster trend is used to display different gene expression trend with multiple lines showing the similar expression patterns in each cluster.

## Required R Packages
- Mfuzz
- RColorBrewer
- data.table
- ggplotify
- jsonlite

## Minimal Reproducible Code
```r
# Load packages
library(Mfuzz)
library(RColorBrewer)
library(data.table)
library(ggplotify)
library(jsonlite)

# Prepare data
# Load data
data <- data.table::fread(jsonlite::read_json("https://hiplot.cn/ui/basic/gene-trend/data.json")$exampleData$textarea[[1]])
data <- as.data.frame(data)

# Convert data structure
## Convert a gene expression matrix to an ExpressionSet object
row.names(data) <- data[,1]
data <- data[,-1]
data <- as.matrix(data)
eset <- new("ExpressionSet", exprs = data)
## Filter genes with more than 25% missing values
eset <- filter.NA(eset, thres=0.25)
## Remove genes with small differences between samples based on standard deviation
eset <- filter.std(eset, min.std=0, visu = F)
## Data Standardization
eset <- standardise(eset)
## Set the number of clusters
c <- 6
## Evaluate the optimal m value
m <- mestimate(eset)
## Perform mfuzz clustering
cl <- mfuzz(eset, c = c, m = m)

# View data
head(data)

# Create visualization
# Gene Cluster Trend
p <- as.ggplot(function(){
  mfuzz.plot2(
  eset,
  cl,
  xlab = "Time",
  ylab = "Expression changes",
  mfrow = c(2,(c/2+0.5)),
  colo = "fancy",
  centre = T,
  centre.col = "red",
  time.labels = colnames(eset),
  x11=F)
  })

# ... (see full tutorial for more)
```

## Key Parameters
- `fill`: Maps a variable to fill color for group comparison
- `color`: Maps a variable to outline/point color

## Tips
- Adjust text size with `theme(text = element_text(size = 14))` for presentations
- See the full tutorial for additional customization options and advanced examples

## Full Tutorial
https://openbiox.github.io/Bizard/Hiplot/062-gene-trend.html
