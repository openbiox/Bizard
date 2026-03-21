# Skill: Barplot (R)

## Category
Hiplot

## When to Use
Bar charts are used to display category data with rectangular bars whose height or length is proportional to the value they represent. Bar charts can be drawn vertically or horizontally. The bar chart shows the comparison between the discrete categories. One axis of the chart shows the specific categories to be compared, and the other axis represents the measurements. Some bar charts show bars that can also show the values of multiple measurement variables.

## Required R Packages
- data.table
- ggplot2
- ggthemes
- jsonlite

## Minimal Reproducible Code
```r
# Load packages
library(data.table)
library(ggplot2)
library(ggthemes)
library(jsonlite)

# Prepare data
# Load data
data <- data.table::fread(jsonlite::read_json("https://hiplot.cn/ui/basic/barplot/data.json")$exampleData$textarea[[1]])
data <- as.data.frame(data)

# convert data structure
data[, 2] <- factor(data[, 2], levels = unique(data[, 2]))
data[, 3] <- factor(data[, 3], levels = unique(data[, 3]))

# View data
head(data)

# Create visualization
# Barplot
p <- ggplot(data, aes(x = dose, y = value, fill = treat)) +
  geom_bar(position = position_dodge(0.9), stat = "identity") +
  ggtitle("Bar Plot") +
  geom_text(aes(label = value), position = position_dodge(0.9), vjust = 1.5, color = "white", size = 3.5) +
  scale_fill_manual(values = c("#e04d39","#5bbad6","#1e9f86","#3c5488ff")) +
  theme_stata() +
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
- `x`: Maps `dose` to the x aesthetic
- `y`: Maps `value` to the y aesthetic
- `fill`: Maps `treat` to the fill aesthetic
- `position`: Position adjustment (identity, dodge, stack, fill)
- `stat`: Statistical transformation to use
- `theme`: Plot theme; tutorial uses `theme_stata()`

## Tips
- Customize color scales with `scale_fill_manual()` or `scale_color_brewer()`
- Adjust text size with `theme(text = element_text(size = 14))` for presentations
- See the full tutorial for additional customization options and advanced examples

## Full Tutorial
https://openbiox.github.io/Bizard/Hiplot/010-barplot.html
