# Skill: Piestats (R)

## Category
Hiplot

## When to Use
Create a Piestats using R with the Hiplot platform's approach. Suitable for biomedical data visualization with publication-quality output.

## Required R Packages
- data.table
- ggplot2
- ggstatsplot
- jsonlite

## Minimal Reproducible Code
```r
# Load packages
library(data.table)
library(ggplot2)
library(ggstatsplot)
library(jsonlite)

# Prepare data
# Load data
data <- data.table::fread(jsonlite::read_json("https://hiplot.cn/ui/basic/ggpiestats/data.json")$exampleData$textarea[[1]])
data <- as.data.frame(data)

# Convert data structure
axis <- c("am", "cyl")
data[, axis[1]] <- factor(data[, axis[1]], levels = unique(data[, axis[1]]))
data[, axis[2]] <- factor(data[, axis[2]], levels = unique(data[, axis[2]]))

# View data
head(data)

# Create visualization
# Piestats
p <- ggpiestats(data = data, x = am, y = cyl,
                paired = F) +
  scale_fill_manual(values = c("#3B4992FF","#EE0000FF"))

p
```

## Key Parameters
- `stat`: Statistical transformation to use
- `fill`: Maps a variable to fill color for group comparison
- `color`: Maps a variable to outline/point color

## Tips
- Customize color scales with `scale_fill_manual()` or `scale_color_brewer()`
- Adjust text size with `theme(text = element_text(size = 14))` for presentations
- See the full tutorial for additional customization options and advanced examples

## Full Tutorial
https://openbiox.github.io/Bizard/Hiplot/071-ggpiestats.html
