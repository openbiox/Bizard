# Skill: Dumbbell Chart (R)

## Category
Hiplot

## When to Use
Dumbbell Chart can display the data change.

## Required R Packages
- data.table
- ggalt
- ggplot2
- jsonlite

## Minimal Reproducible Code
```r
# Load packages
library(data.table)
library(ggalt)
library(ggplot2)
library(jsonlite)

# Prepare data
# Load data
data <- data.table::fread(jsonlite::read_json("https://hiplot.cn/ui/basic/dumbbell/data.json")$exampleData$textarea[[1]])
data <- as.data.frame(data)

# View data
head(data)

# Create visualization
# Dumbbell Chart
colors <- c("#3B4992FF","#EE0000FF")
p <- ggplot(data, aes(y = reorder(country, y1952), x = y1952, xend = y2007)) +
  geom_dumbbell(size = 1, size_x = 3,  size_xend = 3, colour = "#AFAFAF",  
                colour_x = colors[1],  colour_xend = colors[2]) +
  labs(title = "Dummbbell Chart", x = "Life Expectancy (years)",
       y = "country") +
  theme_minimal() +
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
- `y`: Maps `reorder` to the y aesthetic
- `position`: Position adjustment (identity, dodge, stack, fill)
- `theme`: Plot theme; tutorial uses `theme_minimal()`

## Tips
- Use `theme_minimal()` or `theme_bw()` for clean, publication-ready plots
- Adjust text size with `theme(text = element_text(size = 14))` for presentations
- See the full tutorial for additional customization options and advanced examples

## Full Tutorial
https://openbiox.github.io/Bizard/Hiplot/048-dumbbell.html
