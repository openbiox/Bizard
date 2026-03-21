# Skill: Circular Pie Chart (R)

## Category
Hiplot

## When to Use
Another form of the pie chart.

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
data <- data.table::fread(jsonlite::read_json("https://hiplot.cn/ui/basic/circular-pie-chart/data.json")$exampleData$textarea[[1]])
data <- as.data.frame(data)

# convert data structure
data$draw_percent <- data[["values"]] / sum(data[["values"]]) * 100
data$draw_class <- 1
data2 <- data
data2[["values"]] <- 0
data2$draw_class <- 0
data <- rbind(data, data2)
filtered_data <- data[data[["values"]] > 0,]

# View data
head(data)

# Create visualization
# Circular Pie Chart
p <- ggplot(data, aes(x = draw_class, y = values, fill = labels)) +
  geom_bar(position = "stack", stat = "identity", width = 0.7) +
  geom_text(data = filtered_data, aes(label = sprintf("%.2f%%", draw_percent)),
            position = position_stack(vjust = 0.5), size = 3) +
  coord_polar(theta = "y") +
  xlab("") +
  ylab("Pie Chart") +
  scale_fill_manual(values = c("#e64b35ff","#4dbbd5ff","#00a087ff","#3c5488ff","#f39b7fff")) +
  theme_minimal() +
  theme(text = element_text(family = "Arial"),
        plot.title = element_text(size = 12,hjust = 0.5),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 10),
        axis.text.x = element_text(color = "black"),
        axis.text.y = element_blank(),
        legend.position = "right",
        legend.direction = "vertical",
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 10),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

p
```

## Key Parameters
- `x`: Maps `draw_class` to the x aesthetic
- `y`: Maps `values` to the y aesthetic
- `fill`: Maps `labels` to the fill aesthetic
- `width`: Controls element width
- `position`: Position adjustment (identity, dodge, stack, fill)
- `stat`: Statistical transformation to use
- `theme`: Plot theme; tutorial uses `theme_minimal()`

## Tips
- Use `theme_minimal()` or `theme_bw()` for clean, publication-ready plots
- Customize color scales with `scale_fill_manual()` or `scale_color_brewer()`
- See the full tutorial for additional customization options and advanced examples

## Full Tutorial
https://openbiox.github.io/Bizard/Hiplot/024-circular-pie-chart.html
