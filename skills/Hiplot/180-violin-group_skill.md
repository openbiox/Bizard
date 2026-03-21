# Skill: Violin Group (R)

## Category
Hiplot

## When to Use
Violin and box plot of grouped data with T-test.

## Required R Packages
- data.table
- ggpubr
- ggthemes
- jsonlite

## Minimal Reproducible Code
```r
# Load packages
library(data.table)
library(ggpubr)
library(ggthemes)
library(jsonlite)

# Prepare data
# Load data
data <- data.table::fread(jsonlite::read_json("https://hiplot.cn/ui/basic/violin-group/data.json")$exampleData$textarea[[1]])
data <- as.data.frame(data)

# convert data structure
data[, 3] <- factor(data[, 3], levels = unique(data[, 3]))

# View data
head(data)

# Create visualization
# Violin Group
p <- ggviolin(data, x = "Group1", y = "Value", color = "Group2", add = "dotplot",
              add.params = list(fill = "white",size = 1), title = "Violin Diagram",
              xlab = "Group1", ylab = "Value", fill = "Group2",
              palette = c("#374E55FF", "#DF8F44FF"), alpha = 0.5, trim = F) +
  stat_compare_means(aes(group = data[, colnames(data)[3]]),
    method = "t.test", vjust = -6, label.x.npc = "left", label.y.npc = "top",
    tip.length = 0.03, bracket.size = 0.3, step.increase = 0, position = "identity",
    na.rm = FALSE, show.legend = NA, inherit.aes = TRUE, geom = "text") +
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
- `group`: Maps `data` to the group aesthetic
- `alpha`: Controls transparency (0 = fully transparent, 1 = opaque)
- `position`: Position adjustment (identity, dodge, stack, fill)
- `stat`: Statistical transformation to use
- `theme`: Plot theme; tutorial uses `theme_stata()`

## Tips
- Adjust text size with `theme(text = element_text(size = 14))` for presentations
- See the full tutorial for additional customization options and advanced examples

## Full Tutorial
https://openbiox.github.io/Bizard/Hiplot/180-violin-group.html
