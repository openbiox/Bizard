# Skill: Group Dumbbell (R)

## Category
Hiplot

## When to Use
Create a Group Dumbbell using R with the Hiplot platform's approach. Suitable for biomedical data visualization with publication-quality output.

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
data <- data.table::fread(jsonlite::read_json("https://hiplot.cn/ui/basic/group-dumbbell/data.json")$exampleData$textarea[[1]])
data <- as.data.frame(data)

# Convert data structure
data <- data[order(data[["group"]], data[["y1952"]]),]
data[["country"]] <- factor(data[["country"]], levels = data[["country"]])

# View data
head(data)

# Create visualization
# Group Dumbbell
p <- ggplot(data = data, aes(x = y1952, xend = y2007, y = country, color = group)) +
  geom_dumbbell(size = 1, size_xend = 2, size_x = 2) +
  theme_bw()

p
```

## Key Parameters
- `x`: Maps `y1952` to the x aesthetic
- `y`: Maps `country` to the y aesthetic
- `color`: Maps `group` to the color aesthetic
- `theme`: Plot theme; tutorial uses `theme_bw()`

## Tips
- Use `theme_minimal()` or `theme_bw()` for clean, publication-ready plots
- Adjust text size with `theme(text = element_text(size = 14))` for presentations
- See the full tutorial for additional customization options and advanced examples

## Full Tutorial
https://openbiox.github.io/Bizard/Hiplot/083-group-dumbbell.html
