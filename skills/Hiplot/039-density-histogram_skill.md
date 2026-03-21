# Skill: Density-Histogram (R)

## Category
Hiplot

## When to Use
Use density plots or histograms to show data distribution.

## Required R Packages
- data.table
- dplyr
- grafify
- jsonlite

## Minimal Reproducible Code
```r
# Load packages
library(data.table)
library(dplyr)
library(grafify)
library(jsonlite)

# Prepare data
# Load data
data <- data.table::fread(jsonlite::read_json("https://hiplot.cn/ui/basic/density-histogram/data.json")$exampleData[[1]]$textarea[[1]])
data <- as.data.frame(data)

# convert data structure
y <- "Doubling_time"
group <- "Student"
data[, group] <- factor(data[, group], levels = unique(data[, group]))
data <- data %>% 
  mutate(median = median(get(y), na.rm = TRUE),
         mean = mean(get(y), na.rm = TRUE))

# View data
head(data)

# Create visualization
# Density Plot
p <- plot_density(
  data = data, 
  ycol = get(y), 
  group = get(group),
  linethick = 0.5,
  c_alpha = 0.6) + 
  ggtitle("Density Plot") + 
  geom_vline(aes_string(xintercept = "median"),
        colour = 'black', linetype = 2, size = 0.5) + 
  xlab(y) + 
  ylab("density") + 
  guides(fill = guide_legend(title = group), color = FALSE) +
  theme(text = element_text(family = "Arial"),
        plot.title = element_text(size = 12,hjust = 0.5),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 10),
        axis.text.x = element_text(angle = 0, hjust = 0.5,vjust = 1),
        legend.position = "bottom",
        legend.direction = "horizontal",
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 10))

p
```

## Key Parameters
- `alpha`: Controls transparency (0 = fully transparent, 1 = opaque)
- `position`: Position adjustment (identity, dodge, stack, fill)
- `fill`: Maps a variable to fill color for group comparison
- `color`: Maps a variable to outline/point color

## Tips
- Adjust text size with `theme(text = element_text(size = 14))` for presentations
- See the full tutorial for additional customization options and advanced examples

## Full Tutorial
https://openbiox.github.io/Bizard/Hiplot/039-density-histogram.html
