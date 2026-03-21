# Skill: DIY GSEA (R)

## Category
Hiplot

## When to Use
Create a DIY GSEA using R with the Hiplot platform's approach. Suitable for biomedical data visualization with publication-quality output.

## Required R Packages
- clusterProfiler
- data.table
- jsonlite

## Minimal Reproducible Code
```r
# Load packages
library(clusterProfiler)
library(data.table)
library(jsonlite)

# Prepare data
# Load data
data1 <- data.table::fread(jsonlite::read_json("https://hiplot.cn/ui/basic/diy-gsea/data.json")$exampleData$textarea[[1]])
data1 <- as.data.frame(data1)
data2 <- data.table::fread(jsonlite::read_json("https://hiplot.cn/ui/basic/diy-gsea/data.json")$exampleData$textarea[[2]])
data2 <- as.data.frame(data2)

# convert data structure
data1[,2] <- as.numeric(data1[,2])
geneList <- data1[,2]
names(geneList) <- data1[,1]
geneList <- sort(geneList, decreasing = TRUE)
term <- data.frame(term=data2[,1], gene=data2[,2])

# View data
head(term)

# Create visualization
# DIY GSEA
y <- clusterProfiler::GSEA(geneList, TERM2GENE = term, pvalueCutoff = 1)
p <- gseaplot(
  y,
  y@result$Description[1],
  color = "#000000",
  by = "runningScore",
  color.line = "#4CAF50",
  color.vline= "#FA5860",
  title = "DIY GSEA Plot",
  )

p
```

## Key Parameters
- `fill`: Maps a variable to fill color for group comparison
- `color`: Maps a variable to outline/point color

## Tips
- Adjust text size with `theme(text = element_text(size = 14))` for presentations
- See the full tutorial for additional customization options and advanced examples

## Full Tutorial
https://openbiox.github.io/Bizard/Hiplot/044-diy-gsea.html
