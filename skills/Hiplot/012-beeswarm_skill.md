# Skill: Beeswarm (R)

## Category
Hiplot

## When to Use
The beeswarm is a noninterference scatter plot which is similar to a bee colony.

## Required R Packages
- data.table
- ggbeeswarm
- ggthemes
- jsonlite

## Minimal Reproducible Code
```r
# Load packages
library(data.table)
library(ggbeeswarm)
library(ggthemes)
library(jsonlite)

# Prepare data
# Load data
data <- data.table::fread(jsonlite::read_json("https://hiplot.cn/ui/basic/beeswarm/data.json")$exampleData$textarea[[1]])
data <- as.data.frame(data)

# convert data structure
data[, 1] <- factor(data[, 1], levels = unique(data[, 1]))
colnames(data) <- c("Group", "y")

# View data
head(data)

# Create visualization
# Beeswarm
p <- ggplot(data, aes(Group, y, color = Group)) +
  geom_beeswarm(alpha = 1, size = 0.8) +
  labs(x = NULL, y = "value") +
  ggtitle("BeeSwarm Plot") +
  scale_color_manual(values = c("#e04d39","#5bbad6","#1e9f86")) +
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
- `color`: Maps `Group` to the color aesthetic
- `alpha`: Controls transparency (0 = fully transparent, 1 = opaque)
- `position`: Position adjustment (identity, dodge, stack, fill)
- `stat`: Statistical transformation to use
- `theme`: Plot theme; tutorial uses `theme_stata()`

## Tips
- Customize color scales with `scale_fill_manual()` or `scale_color_brewer()`
- Adjust text size with `theme(text = element_text(size = 14))` for presentations
- See the full tutorial for additional customization options and advanced examples

## Full Tutorial
https://openbiox.github.io/Bizard/Hiplot/012-beeswarm.html
