# Skill: Custom Heatmap (R)

## Category
Hiplot

## When to Use
Custom Heatmap, directly plot a heatmap based on the given data.

## Required R Packages
- data.table
- ggplot2
- jsonlite

## Minimal Reproducible Code
```r
# Load packages
library(data.table)
library(ggplot2)
library(jsonlite)

# Prepare data
# Load data
data <- data.table::fread(jsonlite::read_json("https://hiplot.cn/ui/basic/custom-heat-map/data.json")$exampleData$textarea[[1]])
data <- as.data.frame(data)

# convert data structure
draw_data <- as.matrix(data[, 2:ncol(data)])
row_num <- nrow(draw_data)
col_num <- ncol(draw_data)
col_labels <- colnames(data)
col_labels <- col_labels[2:ncol(data)]
row_labels <- data$name
rm(data)
df <- expand.grid(row = 1:row_num, col = 1:col_num)
df$value <- c(draw_data)

# View data
head(df)

# Create visualization
# Custom Heatmap
p <- ggplot(df, aes(x = col, y = row, fill = value)) +
  geom_point(shape = 21, size = 8, aes(fill = value), color = "white") +
  scale_fill_gradient(low = "#DDDDDD", high = "#0000F5") +
  guides(fill = guide_colorbar(title = "Value")) +
  theme(
    panel.background = element_rect(fill = "white"),
    panel.grid = element_blank(),
    axis.text = element_text(size = 10),
    axis.ticks = element_blank(),
    axis.title = element_blank()
    ) +
  scale_x_continuous(breaks = 1:col_num, labels = col_labels, position = "top") +
  scale_y_reverse(breaks = 1:row_num, labels = row_labels, position = "left")

p
```

## Key Parameters
- `x`: Maps `col` to the x aesthetic
- `y`: Maps `row` to the y aesthetic
- `fill`: Maps `value` to the fill aesthetic
- `position`: Position adjustment (identity, dodge, stack, fill)

## Tips
- Customize color scales with `scale_fill_manual()` or `scale_color_brewer()`
- Adjust text size with `theme(text = element_text(size = 14))` for presentations
- See the full tutorial for additional customization options and advanced examples

## Full Tutorial
https://openbiox.github.io/Bizard/Hiplot/034-custom-heat-map.html
