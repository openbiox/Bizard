# Skill: Parliament (R)

## Category
Hiplot

## When to Use
The parliamentary chart is a data processing method that looks like a parliamentary seat, with points representing a data set to show the share ratio of each group more flexibly.

## Required R Packages
- data.table
- ggplot2
- ggpol
- jsonlite

## Minimal Reproducible Code
```r
# Load packages
library(data.table)
library(ggplot2)
library(ggpol)
library(jsonlite)

# Prepare data
# Load data
data <- data.table::fread(jsonlite::read_json("https://hiplot.cn/ui/basic/parliament/data.json")$exampleData$textarea[[1]])
data <- as.data.frame(data)

# View data
head(data)

# Create visualization
# Parliament
p <- ggplot(data) +
  geom_parliament(alpha = 1, aes(seats = value, fill = group), color = "black") +
  coord_fixed() +
  scale_fill_discrete(name = "group", labels = unique(data$group)) +
  scale_fill_manual(values = c("#E64B35FF","#4DBBD5FF","#00A087FF","#3C5488FF",
                                "#F39B7FFF")) +
  ggtitle("Parliament Plot") +
  theme_void() +
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = 0.5))

p
```

## Key Parameters
- `fill`: Maps `group` to the fill aesthetic
- `alpha`: Controls transparency (0 = fully transparent, 1 = opaque)
- `position`: Position adjustment (identity, dodge, stack, fill)
- `theme`: Plot theme; tutorial uses `theme_void()`

## Tips
- Customize color scales with `scale_fill_manual()` or `scale_color_brewer()`
- Adjust text size with `theme(text = element_text(size = 14))` for presentations
- See the full tutorial for additional customization options and advanced examples

## Full Tutorial
https://openbiox.github.io/Bizard/Hiplot/134-parliament.html
