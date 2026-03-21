# Skill: Multiple Barplot&Line (R)

## Category
Hiplot

## When to Use
Displaying multiple bar or line plot in one diagram.

## Required R Packages
- data.table
- ggplot2
- ggthemes
- jsonlite
- reshape2

## Minimal Reproducible Code
```r
# Load packages
library(data.table)
library(ggplot2)
library(ggthemes)
library(jsonlite)
library(reshape2)

# Prepare data
# Load data
data <- data.table::fread(jsonlite::read_json("https://hiplot.cn/ui/basic/barplot-line-multiple/data.json")$exampleData$textarea[[1]])
data <- as.data.frame(data)

# convert data structure
data_melt <- melt(data, id.vars = colnames(data)[1])
data_melt[, 1] <- factor(data_melt[, 1], level = unique(data_melt[, 1]))

# View data
head(data)

# Create visualization
# Multiple Line
p <- ggplot(data = data_melt, aes(x = age, y = value, group = variable,
                                  colour = variable)) +
  geom_line(alpha = 1, size = 1) +
  geom_point(aes(shape = variable), alpha = 1, size = 3) +
  labs(title = "Line (Multiple)", x = "X Lable", y = "Value") +
  scale_color_manual(values = c("#3B4992FF","#EE0000FF","#008B45FF","#631879FF",
                               "#008280FF","#BB0021FF")) +
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
- `x`: Maps `age` to the x aesthetic
- `y`: Maps `value` to the y aesthetic
- `group`: Maps `variable` to the group aesthetic
- `colour`: Maps `variable` to the colour aesthetic
- `alpha`: Controls transparency (0 = fully transparent, 1 = opaque)
- `position`: Position adjustment (identity, dodge, stack, fill)
- `stat`: Statistical transformation to use
- `theme`: Plot theme; tutorial uses `theme_stata()`

## Tips
- Customize color scales with `scale_fill_manual()` or `scale_color_brewer()`
- Adjust text size with `theme(text = element_text(size = 14))` for presentations
- See the full tutorial for additional customization options and advanced examples

## Full Tutorial
https://openbiox.github.io/Bizard/Hiplot/009-barplot-line-multiple.html
