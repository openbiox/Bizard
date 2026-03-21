# Skill: Parallel Coordinate (R)

## Category
Hiplot

## When to Use
Create a Parallel Coordinate using R with the Hiplot platform's approach. Suitable for biomedical data visualization with publication-quality output.

## Required R Packages
- GGally
- data.table
- ggthemes
- hrbrthemes
- jsonlite
- viridis

## Minimal Reproducible Code
```r
# Load packages
library(GGally)
library(data.table)
library(ggthemes)
library(hrbrthemes)
library(jsonlite)
library(viridis)

# Prepare data
# Load data
data <- data.table::fread(jsonlite::read_json("https://hiplot.cn/ui/basic/parallel-coordinate/data.json")$exampleData$textarea[[1]])
data <- as.data.frame(data)

# Convert data structure
data[, 6] <- factor(data[, 6], levels = unique(data[, 6]))

# View data
head(data)

# Create visualization
# Parallel Coordinate
p <- ggparcoord(data, columns = 2:(ncol(data) - 1), groupColumn = ncol(data),
                title = "Parallel Coordinate Plot for cancer Data",
                alphaLines = 0.3, scale = "globalminmax",
                showPoints = T, boxplot = F) +
  theme_base() +
  theme(text = element_text(family = "Arial"),
        plot.title = element_text(size = 12, hjust = 0.5),
        axis.title = element_text(size = 10),
        axis.text = element_text(size = 12),
        axis.text.x = element_text(angle = 0, hjust = 0.5,vjust = 1),
        legend.position = "right",
        legend.direction = "vertical",
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 10)) +
  scale_color_viridis(discrete = TRUE) +
  facet_grid(formula(paste("~", (colnames(data)[ncol(data)]))))

p
```

## Key Parameters
- `alpha`: Controls transparency (0 = fully transparent, 1 = opaque)
- `position`: Position adjustment (identity, dodge, stack, fill)
- `theme`: Plot theme; tutorial uses `theme_base()`

## Tips
- Customize color scales with `scale_fill_manual()` or `scale_color_brewer()`
- Use `facet_wrap()` or `facet_grid()` to create multi-panel plots by group
- See the full tutorial for additional customization options and advanced examples

## Full Tutorial
https://openbiox.github.io/Bizard/Hiplot/132-parallel-coordinate.html
