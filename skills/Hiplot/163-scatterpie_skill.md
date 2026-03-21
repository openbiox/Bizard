# Skill: Scatterpie (R)

## Category
Hiplot

## When to Use
Scatter Pie can be used to visualize data fraction in different space coordinates.

## Required R Packages
- data.table
- jsonlite
- scatterpie

## Minimal Reproducible Code
```r
# Load packages
library(data.table)
library(jsonlite)
library(scatterpie)

# Prepare data
# Load data
data <- data.table::fread(jsonlite::read_json("https://hiplot.cn/ui/basic/scatterpie/data.json")$exampleData$textarea[[1]])
data <- as.data.frame(data)

# View data
head(data)

# Create visualization
# Scatterpie
p <- ggplot() +
  geom_scatterpie(data = data, aes(x = x, y = y), cols = colnames(data)[-c(1, 2)]) +
  scale_fill_manual(values = c("#E64B35FF","#4DBBD5FF","#00A087FF","#3C5488FF")) +
  labs(x="x", y="y") +
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
- `x`: Maps `x` to the x aesthetic
- `y`: Maps `y` to the y aesthetic
- `position`: Position adjustment (identity, dodge, stack, fill)
- `theme`: Plot theme; tutorial uses `theme_minimal()`

## Tips
- Use `theme_minimal()` or `theme_bw()` for clean, publication-ready plots
- Customize color scales with `scale_fill_manual()` or `scale_color_brewer()`
- See the full tutorial for additional customization options and advanced examples

## Full Tutorial
https://openbiox.github.io/Bizard/Hiplot/163-scatterpie.html
