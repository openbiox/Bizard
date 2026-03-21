# Skill: Deviation Plot (R)

## Category
Hiplot

## When to Use
Deviation plot provides a visual representation of the differences between data points.

## Required R Packages
- data.table
- ggpubr
- jsonlite

## Minimal Reproducible Code
```r
# Load packages
library(data.table)
library(ggpubr)
library(jsonlite)

# Prepare data
# Load data
data <- data.table::fread(jsonlite::read_json("https://hiplot.cn/ui/basic/deviation-plot/data.json")$exampleData$textarea[[1]])
data <- as.data.frame(data)

# convert data structure
data[["z_score"]] <- (data[["mpg"]] - mean(data[["mpg"]])) / sd(data[["mpg"]])
data[["Group"]] <- factor(ifelse(data[["z_score"]] < 0, "low", "high"),
                          levels = c("low", "high")
                          )

# View data
head(data)

# Create visualization
# Deviation Plot
p <- ggbarplot(data,
    x = "name",
    y = "z_score",
    fill = "Group",
    color = "white",
    sort.val = "desc",
    sort.by.groups = FALSE,
    x.text.angle = 90,
    xlab = "name",
    ylab = "mpg",
    rotate = TRUE
  ) +
  scale_fill_manual(values = c("#e04d39","#5bbad6")) +
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
- `position`: Position adjustment (identity, dodge, stack, fill)
- `theme`: Plot theme; tutorial uses `theme_bw()`
- `fill`: Maps a variable to fill color for group comparison
- `color`: Maps a variable to outline/point color

## Tips
- Use `theme_minimal()` or `theme_bw()` for clean, publication-ready plots
- Customize color scales with `scale_fill_manual()` or `scale_color_brewer()`
- See the full tutorial for additional customization options and advanced examples

## Full Tutorial
https://openbiox.github.io/Bizard/Hiplot/041-deviation-plot.html
