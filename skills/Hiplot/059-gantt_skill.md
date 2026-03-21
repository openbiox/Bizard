# Skill: Gantt (R)

## Category
Hiplot

## When to Use
The Gantt chart is a type of bar chart that illustrates a project schedule.

## Required R Packages
- data.table
- ggthemes
- jsonlite
- tidyverse

## Minimal Reproducible Code
```r
# Load packages
library(data.table)
library(ggthemes)
library(jsonlite)
library(tidyverse)

# Prepare data
# Load data
data <- data.table::fread(jsonlite::read_json("https://hiplot.cn/ui/basic/gantt/data.json")$exampleData$textarea[[1]])
data <- as.data.frame(data)

# Convert data structure
usr_ylab <- colnames(data)[1]
if (!is.numeric(data[, 2])) {
  data[, 2] <- factor(data[, 2], levels = unique(data[, 2]))
}
data_gather <- gather(data, "state", "date", 3:4)
sample <- levels(data_gather$sample)
data_gather$sample <- factor(data_gather$sample,
  levels = rev(unique(data_gather$sample))
)

# View data
head(data_gather)

# Create visualization
# Gantt
p <- ggplot(data_gather, aes(date, sample, color = item)) +
  geom_line(size = 10, alpha = 1) +
  labs(x = "Time", y = "sample", title = "Gantt Plot") +
  theme(axis.ticks = element_blank()) +
  scale_color_manual(values = c("#e04d39","#5bbad6","#1e9f86")) +
  theme_stata() +
  theme(text = element_text(family = "Arial"),
        plot.title = element_text(size = 12,hjust = 0.5),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 10),
        axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 1),
        legend.position = "right",
        legend.direction = "vertical",
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 10))

p
```

## Key Parameters
- `color`: Maps `item` to the color aesthetic
- `alpha`: Controls transparency (0 = fully transparent, 1 = opaque)
- `position`: Position adjustment (identity, dodge, stack, fill)
- `stat`: Statistical transformation to use
- `theme`: Plot theme; tutorial uses `theme_stata()`

## Tips
- Customize color scales with `scale_fill_manual()` or `scale_color_brewer()`
- Adjust text size with `theme(text = element_text(size = 14))` for presentations
- See the full tutorial for additional customization options and advanced examples

## Full Tutorial
https://openbiox.github.io/Bizard/Hiplot/059-gantt.html
