# Skill: Group Line (R)

## Category
Hiplot

## When to Use
Create a Group Line using R with the Hiplot platform's approach. Suitable for biomedical data visualization with publication-quality output.

## Required R Packages
- data.table
- ggplot2
- jsonlite

## Minimal Reproducible Code
```r
# Load packages
library(data.table)
library(ggplot2)
library(jsonlite)

# Prepare data
# Load data
data <- data.table::fread(jsonlite::read_json("https://hiplot.cn/ui/basic/group-line/data.json")$exampleData$textarea[[1]])
data <- as.data.frame(data)

# View data
head(data)

# Create visualization
# Group Line
p <- ggplot(data, aes(x = x, y = y, group = names, color = groups)) +
  geom_line() +
  geom_point() +
  scale_color_manual(values = c("#e04d39","#5bbad6")) +
  theme_bw()

p
```

## Key Parameters
- `x`: Maps `x` to the x aesthetic
- `y`: Maps `y` to the y aesthetic
- `group`: Maps `names` to the group aesthetic
- `color`: Maps `groups` to the color aesthetic
- `theme`: Plot theme; tutorial uses `theme_bw()`

## Tips
- Use `theme_minimal()` or `theme_bw()` for clean, publication-ready plots
- Customize color scales with `scale_fill_manual()` or `scale_color_brewer()`
- See the full tutorial for additional customization options and advanced examples

## Full Tutorial
https://openbiox.github.io/Bizard/Hiplot/084-group-line.html
