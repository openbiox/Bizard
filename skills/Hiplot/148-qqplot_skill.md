# Skill: QQ Plot (R)

## Category
Hiplot

## When to Use
Verify whether a set of data comes from a certain distribution or whether two sets of data come from the same (family) distribution.

## Required R Packages
- data.table
- grafify
- jsonlite

## Minimal Reproducible Code
```r
# Load packages
library(data.table)
library(grafify)
library(jsonlite)

# Prepare data
# Load data
data <- data.table::fread(jsonlite::read_json("https://hiplot.cn/ui/basic/qqplot/data.json")$exampleData[[1]]$textarea[[1]])
data <- as.data.frame(data)

# Convert data structure
data[, "Genotype"] <- factor(data[, "Genotype"], levels = unique(data[, "Genotype"]))

# View data
head(data)

# Create visualization
# QQ Plot
p <- plot_qqline(data = data, ycol = Cytokine, group = Genotype,
                 symsize = 2, symthick = 0.5, s_alpha = 1) +
  ggtitle("QQplot without facet") +
  xlab("theoretical") + ylab("sample") + 
  guides(fill = guide_legend(title = "Genotype")) +
  scale_color_manual(values = c("#E69F00","#4DB1DC")) +
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
- Customize color scales with `scale_fill_manual()` or `scale_color_brewer()`
- Adjust text size with `theme(text = element_text(size = 14))` for presentations
- See the full tutorial for additional customization options and advanced examples

## Full Tutorial
https://openbiox.github.io/Bizard/Hiplot/148-qqplot.html
