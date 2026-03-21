# Skill: Bubble (R)

## Category
Hiplot

## When to Use
The bubble chart is a statistical chart that shows the third variable by the size of the bubble on the basis of the scatter chart, so that the three variables can be compared and analyzed simultaneously.

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
data <- data.table::fread(jsonlite::read_json("https://hiplot.cn/ui/basic/bubble/data.json")$exampleData$textarea[[1]])
data <- as.data.frame(data)

# convert data structure
data[, 1] <- str_to_sentence(str_remove(data[, 1], pattern = "\\w+:\\d+\\W"))
topnum <- 7
data <- data[1:topnum, ]
data[, 1] <- factor(data[, 1], level = rev(unique(data[, 1])))

# View data
head(data)

# Create visualization
# Bubble
p <- ggplot(data, aes(Ratio, Term)) +
  geom_point(aes(size = Count, colour = -log10(PValue))) +
  scale_colour_gradient(low = "#00438E", high = "#E43535") +
  labs(colour = "-log10 (PValue)", size = "Count", x = "Ratio", y = "Term", 
       title = "Bubble Plot") +
  scale_x_continuous(limits = c(0, max(data$Ratio) * 1.2)) +
  guides(color = guide_colorbar(order = 1), size = guide_legend(order = 2)) +
  scale_y_discrete(labels = function(x) {str_wrap(x, width = 65)}) +
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
- `size`: Maps `Count` to the size aesthetic
- `width`: Controls element width
- `position`: Position adjustment (identity, dodge, stack, fill)
- `theme`: Plot theme; tutorial uses `theme_bw()`

## Tips
- Use `theme_minimal()` or `theme_bw()` for clean, publication-ready plots
- Adjust text size with `theme(text = element_text(size = 14))` for presentations
- See the full tutorial for additional customization options and advanced examples

## Full Tutorial
https://openbiox.github.io/Bizard/Hiplot/016-bubble.html
