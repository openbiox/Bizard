# Skill: Donut (R)

## Category
Hiplot

## When to Use
The donut is a variant of the pie chart, with a blank center allowing for additional information about the data as a whole to be included. Doughnut charts are similar to pie charts in that their aim is to illustrate proportions.

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
data <- data.table::fread(jsonlite::read_json("https://hiplot.cn/ui/basic/donut/data.json")$exampleData$textarea[[1]])
data <- as.data.frame(data)

# convert data structure
data$fraction <- data[, 2] / sum(data[, 2])
data$ymax <- cumsum(data$fraction)
data$ymin <- c(0, head(data$ymax, n = -1))
data$labelPosition <- (data$ymax + data$ymin) / 2
data$label <- paste0(data[, 1], "\n",
  "(", data[, 2], ", ", sprintf("%2.2f%%", 100 * data[, 2] / sum(data[, 2])), ")",
  sep = ""
)

# View data
head(data)

# Create visualization
# Donut
p <- ggplot(data, aes_(ymax = as.name("ymax"), ymin = as.name("ymin"), 
                       xmax = 4, xmin = 3, fill = as.name(colnames(data)[1]))) +
  geom_rect() +
  geom_text(x = 5 + (4 - 5) / 3,
            aes(y = labelPosition, label = label), size = 4) +
  coord_polar(theta = "y") +
  xlim(c(2, 5)) +
  scale_fill_manual(values = c("#00468BCC","#ED0000CC","#42B540CC","#0099B4CC")) +
  ggtitle("Donut Plot") +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "none")

p
```

## Key Parameters
- `y`: Maps `labelPosition` to the y aesthetic
- `position`: Position adjustment (identity, dodge, stack, fill)
- `theme`: Plot theme; tutorial uses `theme_void()`

## Tips
- Customize color scales with `scale_fill_manual()` or `scale_color_brewer()`
- Adjust text size with `theme(text = element_text(size = 14))` for presentations
- See the full tutorial for additional customization options and advanced examples

## Full Tutorial
https://openbiox.github.io/Bizard/Hiplot/045-donut.html
