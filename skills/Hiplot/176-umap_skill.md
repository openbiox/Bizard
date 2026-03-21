# Skill: UMAP (R)

## Category
Hiplot

## When to Use
UMAP is a nonlinear dimensionality reduction algorithm suitable for high-dimensional data reduction to two or three dimensions and visualization. The algorithm can make the t distribution of points with greater similarity closer in the lower dimensional space. For low similarity points, the t distribution is farther away in the low dimensional space.

## Required R Packages
- data.table
- ggpubr
- jsonlite
- umap

## Minimal Reproducible Code
```r
# Load packages
library(data.table)
library(ggpubr)
library(jsonlite)
library(umap)

# Prepare data
# Load data
data1 <- data.table::fread(jsonlite::read_json("https://hiplot.cn/ui/basic/umap/data.json")$exampleData$textarea[[1]])
data1 <- as.data.frame(data1)
data2 <- data.table::fread(jsonlite::read_json("https://hiplot.cn/ui/basic/umap/data.json")$exampleData$textarea[[2]])
data2 <- as.data.frame(data2)

# convert data structure
sample.info <- data2
rownames(data1) <- data1[, 1]
data1 <- as.matrix(data1[, -1])
## umap
set.seed(123)
umap_info <- umap(t(data1))
colnames(umap_info$layout) <- c("UMAP_1", "UMAP_2")
# handle data
umap_data <- data.frame(
  sample = colnames(data1),
  umap_info$layout
)
colorBy <- sample.info[match(colnames(data1), sample.info[, 1]), "Species"]
colorBy <- factor(colorBy, level = colorBy[!duplicated(colorBy)])
umap_data$colorBy = colorBy
shapeBy <- NULL

# View data
head(data1[,1:5])
head(data2)

# Create visualization
# umap
p <- ggscatter(data = umap_data, x = "UMAP_1", y = "UMAP_2", size = 2, 
               palette = "lancet", color = "colorBy") +
  labs(color = "group") +
  ggtitle("UMAP Plot") +
  theme_classic() +
  theme(text = element_text(family = "Arial"),
        plot.title = element_text(size = 12,hjust = 0.5),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 10),
        axis.text.x = element_text(angle = 0, hjust = 0.5,vjust = 1),
        legend.position = "right",
        legend.direction = "vertical",
        legend.title = element_text(size = 10),
# ... (see full tutorial for more)
```

## Key Parameters
- `position`: Position adjustment (identity, dodge, stack, fill)
- `theme`: Plot theme; tutorial uses `theme_classic()`
- `fill`: Maps a variable to fill color for group comparison
- `color`: Maps a variable to outline/point color

## Tips
- Adjust text size with `theme(text = element_text(size = 14))` for presentations
- See the full tutorial for additional customization options and advanced examples

## Full Tutorial
https://openbiox.github.io/Bizard/Hiplot/176-umap.html
