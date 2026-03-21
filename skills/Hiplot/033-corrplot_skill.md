# Skill: Corrplot (R)

## Category
Hiplot

## When to Use
The correlation heat map is a graph that analyzes the correlation between two or more variables.

## Required R Packages
- corrplot
- data.table
- ggcorrplot
- ggplotify
- jsonlite

## Minimal Reproducible Code
```r
# Load packages
library(corrplot)
library(data.table)
library(ggcorrplot)
library(ggplotify)
library(jsonlite)

# Prepare data
# Load data
data <- data.table::fread(jsonlite::read_json("https://hiplot.cn/ui/basic/corrplot/data.json")$exampleData[[1]]$textarea[[1]])
data <- as.data.frame(data)

# convert data structure
data <- data[!is.na(data[, 1]), ]
idx <- duplicated(data[, 1])
data[idx, 1] <- paste0(data[idx, 1], "--dup-", cumsum(idx)[idx])
rownames(data) <- data[, 1]
data <- data[, -1]
str2num_df <- function(x) {
  final <- NULL
  for (i in seq_len(ncol(x))) {
    final <- cbind(final, as.numeric(x[, i]))
  }
  colnames(final) <- colnames(x)
  return(final)
}
tmp <- str2num_df(t(data))
corr <- round(cor(tmp, use = "na.or.complete", method = "pearson"), 3)
p_mat <- round(cor_pmat(tmp, method = "pearson"), 3)

# View data
head(data)

# Create visualization
# Corrplot
p <- as.ggplot(function(){
  corrplot(
    corr, 
    method = "circle", 
    type = "upper",
    tl.col = "black", 
    diag = F,
    col = colorRampPalette(c("#4477AA", "#FFFFFF", "#BB4444"))(200),
    order = "hclust",
    hclust.method = "ward.D2")
  }) +
  xlab("") + ylab("") +
  ggtitle("Cor Heatmap Plot") +
  theme_void() +
  theme(text = element_text(family = "Arial"),
# ... (see full tutorial for more)
```

## Key Parameters
- `position`: Position adjustment (identity, dodge, stack, fill)
- `theme`: Plot theme; tutorial uses `theme_void()`
- `fill`: Maps a variable to fill color for group comparison
- `color`: Maps a variable to outline/point color

## Tips
- Adjust text size with `theme(text = element_text(size = 14))` for presentations
- See the full tutorial for additional customization options and advanced examples

## Full Tutorial
https://openbiox.github.io/Bizard/Hiplot/033-corrplot.html
