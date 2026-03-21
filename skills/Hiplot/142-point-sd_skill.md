# Skill: Point (SD) (R)

## Category
Hiplot

## When to Use
Displaying the standard deviation (SD) of multi-group data.

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
data <- data.table::fread(jsonlite::read_json("https://hiplot.cn/ui/basic/point-sd/data.json")$exampleData[[1]]$textarea[[1]])
data <- as.data.frame(data)

# Convert data structure
y <- "Doubling_time"
group <- "Student"
data[, group] <- factor(data[, group], levels = unique(data[, group]))
data <- data %>% 
  mutate(median = median(get(y), na.rm = TRUE),
         mean = mean(get(y), na.rm = TRUE))

# View data
head(data)

# Create visualization
# Point (SD)
p <- plot_point_sd(data = data, Student, Doubling_time, symsize = 5,
                   symthick = 0.5, s_alpha = 1, ewid = 0, symshape = 21,
                   all_alpha = 0) +
  geom_hline(aes(yintercept = median), colour = 'black', linetype = 2, 
             size = 0.5) +
  xlab(group) + ylab(y) + 
  guides(fill = guide_legend(title = group)) +
  ggtitle("Point-SD") +
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
- `alpha`: Controls transparency (0 = fully transparent, 1 = opaque)
- `position`: Position adjustment (identity, dodge, stack, fill)
- `fill`: Maps a variable to fill color for group comparison
- `color`: Maps a variable to outline/point color

## Tips
- Adjust text size with `theme(text = element_text(size = 14))` for presentations
- See the full tutorial for additional customization options and advanced examples

## Full Tutorial
https://openbiox.github.io/Bizard/Hiplot/142-point-sd.html
