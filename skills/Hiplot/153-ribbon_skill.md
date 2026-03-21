# Skill: Ribbon (R)

## Category
Hiplot

## When to Use
The ribbon diagram is a pattern similar to a ribbon.

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
data <- data.table::fread(jsonlite::read_json("https://hiplot.cn/ui/basic/ribbon/data.json")$exampleData$textarea[[1]])
data <- as.data.frame(data)

# Convert data structure
colnames(data) <- c("group", "xvalue", "yvalue1", "yvalue2")
data$yvalue <- (data$yvalue1 + data$yvalue2) / 2

# View data
head(data)

# Create visualization
# Ribbon
p <- ggplot(data, aes(xvalue, yvalue, fill = group)) +
  geom_ribbon(alpha = 0.2, aes(ymin = yvalue1, ymax = yvalue2)) +
  geom_line(aes(y = yvalue, color = group), lwd = 1) +
  geom_line(aes(y = yvalue1, color = group), linetype = "dotted") +
  geom_line(aes(y = yvalue2, color = group), linetype = "dotted") +
  ylab("y axis value") +
  xlab("x axis value") +
  ggtitle("Ribbon Plot") +
  scale_fill_manual(values = c("#e04d39","#5bbad6")) +
  scale_color_manual(values = c("#e04d39","#5bbad6")) +
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
- `fill`: Maps `group` to the fill aesthetic
- `y`: Maps `yvalue2` to the y aesthetic
- `color`: Maps `group` to the color aesthetic
- `alpha`: Controls transparency (0 = fully transparent, 1 = opaque)
- `position`: Position adjustment (identity, dodge, stack, fill)
- `stat`: Statistical transformation to use
- `theme`: Plot theme; tutorial uses `theme_stata()`

## Tips
- Customize color scales with `scale_fill_manual()` or `scale_color_brewer()`
- Adjust text size with `theme(text = element_text(size = 14))` for presentations
- See the full tutorial for additional customization options and advanced examples

## Full Tutorial
https://openbiox.github.io/Bizard/Hiplot/153-ribbon.html
