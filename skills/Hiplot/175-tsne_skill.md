# Skill: tSNE (R)

## Category
Hiplot

## When to Use
T-sne is a nonlinear dimensionality reduction algorithm suitable for high-dimensional data reduction to two or three dimensions and visualization. The algorithm can make the t distribution of points with greater similarity closer in the lower dimensional space. For low similarity points, the t distribution is farther away in the low dimensional space.

## Required R Packages
- Rtsne
- data.table
- ggpubr
- jsonlite

## Minimal Reproducible Code
```r
# Load packages
library(Rtsne)
library(data.table)
library(ggpubr)
library(jsonlite)

# Prepare data
# Load data
data1 <- data.table::fread(jsonlite::read_json("https://hiplot.cn/ui/basic/tsne/data.json")$exampleData[[1]]$textarea[[1]])
data1 <- as.data.frame(data1)
data2 <- data.table::fread(jsonlite::read_json("https://hiplot.cn/ui/basic/tsne/data.json")$exampleData[[1]]$textarea[[2]])
data2 <- as.data.frame(data2)

# convert data structure
sample.info <- data2
rownames(data1) <- data1[, 1]
data1 <- as.matrix(data1[, -1])
## tsne
set.seed(123)
tsne_info <- Rtsne(t(data1), perplexity = 1, theta = 0.1, check_duplicates = FALSE)
colnames(tsne_info$Y) <- c("tSNE_1", "tSNE_2")
# handle data
tsne_data <- data.frame(
  sample = colnames(data1),
  tsne_info$Y
)
colorBy <- sample.info[match(colnames(data1), sample.info[, 1]), "group"]
colorBy <- factor(colorBy, level = colorBy[!duplicated(colorBy)])
tsne_data$colorBy = colorBy
shapeBy <- NULL

# View data
head(data1)
head(data2)

# Create visualization
# tsne
p <- ggscatter(data = tsne_data, x = "tSNE_1", y = "tSNE_2", size = 2, 
               palette = "lancet", color = "colorBy") +
  labs(color = "group") +
  ggtitle("tSNE Plot1") +
  theme_bw() +
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
- `theme`: Plot theme; tutorial uses `theme_bw()`
- `fill`: Maps a variable to fill color for group comparison
- `color`: Maps a variable to outline/point color

## Tips
- Use `theme_minimal()` or `theme_bw()` for clean, publication-ready plots
- Adjust text size with `theme(text = element_text(size = 14))` for presentations
- See the full tutorial for additional customization options and advanced examples

## Full Tutorial
https://openbiox.github.io/Bizard/Hiplot/175-tsne.html
