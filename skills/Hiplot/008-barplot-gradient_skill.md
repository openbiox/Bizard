# Skill: Barplot Gradient (R)

## Category
Hiplot

## When to Use
It is similar to the bubble chart, but on the basis of the histogram, a color gradient rectangle is used to simultaneously display the visualization of two variables.

## Required R Packages
- data.table
- ggplot2
- jsonlite
- stringr

## Minimal Reproducible Code
```r
# Load packages
library(data.table)
library(ggplot2)
library(jsonlite)
library(stringr)

# Prepare data
# Load data
data <- data.table::fread(jsonlite::read_json("https://hiplot.cn/ui/basic/barplot-gradient/data.json")$exampleData$textarea[[1]])
data <- as.data.frame(data)

# convert data structure
data[, 1] <- str_to_sentence(str_remove(data[, 1], pattern = "\\w+:\\d+\\W"))
topnum <- 7
data <- data[1:topnum, ]
data[, 1] <- factor(data[, 1], level = rev(unique(data[, 1])))

# View data
head(data)

# Create visualization
# Barplot Gradient
p <- ggplot(data, aes(x = Term, y = Count, fill = -log10(PValue))) +
  geom_bar(stat = "identity") +
  ggtitle("GO BarPlot") +
  scale_fill_continuous(low = "#00438E", high = "#E43535") +
  scale_x_discrete(labels = function(x) {str_wrap(x, width = 65)}) +
  labs(fill = "-log10 (PValue)", y = "Term", x = "Count") +
  coord_flip() +
  theme_bw() +
  theme(text = element_text(family = "Arial"),
        plot.title = element_text(size = 12,hjust = 0.5),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 10),
        axis.text.x = element_text(angle = 0, hjust = 0.5),
        legend.position = "right",
        legend.direction = "vertical",
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 10))

p
```

## Key Parameters
- `x`: Maps `Term` to the x aesthetic
- `y`: Maps `Count` to the y aesthetic
- `width`: Controls element width
- `position`: Position adjustment (identity, dodge, stack, fill)
- `stat`: Statistical transformation to use
- `theme`: Plot theme; tutorial uses `theme_bw()`

## Tips
- Use `theme_minimal()` or `theme_bw()` for clean, publication-ready plots
- Use `coord_flip()` for horizontal orientation when labels are long
- Customize color scales with `scale_fill_manual()` or `scale_color_brewer()`

## Full Tutorial
https://openbiox.github.io/Bizard/Hiplot/008-barplot-gradient.html
