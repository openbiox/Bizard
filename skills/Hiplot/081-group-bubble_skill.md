# Skill: Group Bubble (R)

## Category
Hiplot

## When to Use
Create a Group Bubble using R with the Hiplot platform's approach. Suitable for biomedical data visualization with publication-quality output.

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
data <- data.table::fread(jsonlite::read_json("https://hiplot.cn/ui/basic/group-bubble/data.json")$exampleData$textarea[[1]])
data <- as.data.frame(data)

# View data
head(data)

# Create visualization
# Group Bubble
p <- ggplot(data = data, aes(x = Sepal.Length, y = Sepal.Width, 
                             size = Petal.Width, color = Species)) +
  geom_point(alpha = 0.7) +
  scale_size(range = c(1, 4)) +
  scale_color_manual(values = c("#e04d39","#5bbad6","#1e9f86")) +
  theme_bw()

p
```

## Key Parameters
- `x`: Maps `Sepal` to the x aesthetic
- `y`: Maps `Sepal` to the y aesthetic
- `size`: Maps `Petal` to the size aesthetic
- `color`: Maps `Species` to the color aesthetic
- `alpha`: Controls transparency (0 = fully transparent, 1 = opaque)
- `theme`: Plot theme; tutorial uses `theme_bw()`

## Tips
- Use `theme_minimal()` or `theme_bw()` for clean, publication-ready plots
- Customize color scales with `scale_fill_manual()` or `scale_color_brewer()`
- See the full tutorial for additional customization options and advanced examples

## Full Tutorial
https://openbiox.github.io/Bizard/Hiplot/081-group-bubble.html
