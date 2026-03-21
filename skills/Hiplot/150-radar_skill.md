# Skill: Radar (R)

## Category
Hiplot

## When to Use
Radar chart displays multivariable data in the form of two-dimensional charts representing three or more quantitative variables on the axis starting from the same point, so as to visually express the comparison of a research object in multiple parameters.

## Required R Packages
- data.table
- dplyr
- ggplot2
- ggradar
- jsonlite
- scales
- tibble

## Minimal Reproducible Code
```r
# Load packages
library(data.table)
library(dplyr)
library(ggplot2)
library(ggradar)
library(jsonlite)
library(scales)

# Prepare data
# Load data
data <- data.table::fread(jsonlite::read_json("https://hiplot.cn/ui/basic/radar/data.json")$exampleData$textarea[[1]])
data <- as.data.frame(data)

# Convert data structure
data <- as.data.frame(t(data))
colnames(data) <- data[1, ]
data <- data[-1, ]
for (i in seq_len(ncol(data))) {
  data[, i] <- as.numeric(data[, i])
}
data_radar <- data %>%
  rownames_to_column(var = "sample")
data_radar <- data_radar %>% mutate_at(vars(-sample), rescale)

# View data
head(data)

# Create visualization
# Radar
p <- ggradar(data_radar, gridline.max.linetype = 1, group.point.size = 4,
             group.line.width = 1, font.radar = "Arial", fill.alpha = 0.5,
             gridline.min.colour = "grey", gridline.mid.colour = "#007A87",
             gridline.max.colour = "grey") +
  ggtitle("Radar Plot") +
  scale_color_manual(values = c("#E64B35FF","#4DBBD5FF","#00A087FF","#3C5488FF")) +
  theme(text = element_text(family = "Arial"),
        plot.title = element_text(size = 12,hjust = 0.5),
        axis.title = element_blank(),
        axis.text = element_text(size = 10),
        axis.text.x = element_blank(),
        axis.title.y=element_blank(),
        axis.ticks.y=element_blank(),
        legend.position = "right",
        legend.direction = "vertical",
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 10))
 
p
```

## Key Parameters
- `alpha`: Controls transparency (0 = fully transparent, 1 = opaque)
- `width`: Controls element width
- `position`: Position adjustment (identity, dodge, stack, fill)

## Tips
- Customize color scales with `scale_fill_manual()` or `scale_color_brewer()`
- Adjust text size with `theme(text = element_text(size = 14))` for presentations
- See the full tutorial for additional customization options and advanced examples

## Full Tutorial
https://openbiox.github.io/Bizard/Hiplot/150-radar.html
