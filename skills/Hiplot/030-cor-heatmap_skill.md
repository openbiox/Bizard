# Skill: Correlation Heatmap (R)

## Category
Hiplot

## When to Use
The correlation heat map is a graph that analyzes the correlation between two or more variables.

## Required R Packages
- data.table
- ggcorrplot
- jsonlite

## Minimal Reproducible Code
```r
# Load packages
library(data.table)
library(ggcorrplot)
library(jsonlite)

# Prepare data
# Load data
data <- data.table::fread(jsonlite::read_json("https://hiplot.cn/ui/basic/cor-heatmap/data.json")$exampleData$textarea[[1]])
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
# Correlation Heatmap
p <- ggcorrplot(
  corr,
  colors = c("#4477AA", "#FFFFFF", "#BB4444"),
  method = "circle",
  hc.order = T,
  hc.method = "ward.D2",
  outline.col = "white",
  ggtheme = theme_bw(),
  type = "upper",
  lab = F,
  lab_size = 3,
  legend.title = "Correlation"
  ) +
  ggtitle("Cor Heatmap Plot") +
  theme(text = element_text(family = "Arial"),
        plot.title = element_text(size = 12,hjust = 0.5),
        axis.title = element_text(size = 12),
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
https://openbiox.github.io/Bizard/Hiplot/030-cor-heatmap.html
