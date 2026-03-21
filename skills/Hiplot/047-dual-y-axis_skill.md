# Skill: Dual Y Axis Chart (R)

## Category
Hiplot

## When to Use
The dual Y-axis graph can put two groups of data with larger orders of magnitude in the same graph for display.

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
data <- data.table::fread(jsonlite::read_json("https://hiplot.cn/ui/basic/dual-y-axis/data.json")$exampleData$textarea[[1]])
data <- as.data.frame(data)

# View data
head(data)

# Create visualization
# Dual Y Axis Chart
p <- ggplot(data, aes(x = x)) +
  geom_line(aes(y = data[, 2]), size = 1, color = "#D72C15") +
  geom_line(aes(y = data[, 3] / as.numeric(10)), size = 1, color = "#02657B") +
  scale_y_continuous(
    name = colnames(data)[2],
    sec.axis = sec_axis(~ . * as.numeric(10), name = colnames(data)[3])) +
  ggtitle("Dual Y Axis Chart") + xlab("x") +
  theme_bw() +
  theme(text = element_text(family = "Arial"),
        plot.title = element_text(size = 12,hjust = 0.5),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 10),
        axis.text.x = element_text(angle = 0, hjust = 0.5,vjust = 1),
        legend.position = "right",
        legend.direction = "vertical",
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 10))

p
```

## Key Parameters
- `x`: Maps `x` to the x aesthetic
- `y`: Maps `data` to the y aesthetic
- `position`: Position adjustment (identity, dodge, stack, fill)
- `theme`: Plot theme; tutorial uses `theme_bw()`

## Tips
- Use `theme_minimal()` or `theme_bw()` for clean, publication-ready plots
- Adjust text size with `theme(text = element_text(size = 14))` for presentations
- See the full tutorial for additional customization options and advanced examples

## Full Tutorial
https://openbiox.github.io/Bizard/Hiplot/047-dual-y-axis.html
