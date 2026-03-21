# Skill: Venn (R)

## Category
Hiplot

## When to Use
A Venn diagram is a diagramthat shows all possible logical relations between a finite collection of different sets. These diagrams depict elements as points in the plane, and sets as regions inside closed curves. A Venn diagram consists of multiple overlapping closed curves, usually circles, each representing a set. The points inside a curve labelled S represent elements of the set S, while points outside the boundary represent elements not in the set S. This lends to easily read visualizatio...

## Required R Packages
- VennDiagram
- data.table
- jsonlite

## Minimal Reproducible Code
```r
# Load packages
library(VennDiagram)
library(data.table)
library(jsonlite)

# Prepare data
# Load data
data <- data.table::fread(jsonlite::read_json("https://hiplot.cn/ui/basic/venn/data.json")$exampleData$textarea[[1]])
data <- as.data.frame(data)

# convert data structure
for (i in seq_len(ncol(data))) {
  data[is.na(data[, i]), i] <- ""
}
raw <- data
data <- as.data.frame(raw[raw[, 1] != "", 1])
colnames(data) <- colnames(raw)[1]
list.num <- 1
for (i in 2:ncol(raw)) {
  if (any(!is.na(raw[, i]) & raw[, i] != "")) {
    tmp <- raw[i]
    tmp <- tmp[tmp[, 1] != "", ]
    tmp <- as.data.frame(tmp)
    colnames(tmp) <- colnames(raw)[i]
    assign(paste0("data", i), tmp)
    list.num <- list.num + 1
  }
}
colnames(data) <- paste("V", seq_len(ncol(data)), sep = "")
colnames(data2) <- paste("V", seq_len(ncol(data2)), sep = "")
colnames(data3) <- paste("V", seq_len(ncol(data3)), sep = "")
colnames(data4) <- paste("V", seq_len(ncol(data4)), sep = "")
colnames(data5) <- paste("V", seq_len(ncol(data5)), sep = "")
data_list <- list(
  n1 = data$V1, n2 = data2$V1, n3 = data3$V1,
  n4 = data4$V1, n5 = data5$V1
)
names(data_list) <- colnames(raw)[1:5]

# View data
head(data)

# Create visualization
# Venn
col <- c("#E64B35FF","#4DBBD5FF","#00A087FF","#3C5488FF","#F39B7FFF")
p <- venn.diagram(
  data_list, scaled = F, euler.d = F, filename = NULL, col = "black",
  fill = col,
  cex = c(
    1.5, 1.5, 1.5, 1.5, 1.5, 1, 0.8, 1, 0.8, 1, 0.8, 1, 0.8,
# ... (see full tutorial for more)
```

## Key Parameters
- `alpha`: Controls transparency (0 = fully transparent, 1 = opaque)
- `fill`: Maps a variable to fill color for group comparison
- `color`: Maps a variable to outline/point color

## Tips
- Adjust text size with `theme(text = element_text(size = 14))` for presentations
- See the full tutorial for additional customization options and advanced examples

## Full Tutorial
https://openbiox.github.io/Bizard/Hiplot/178-venn.html
