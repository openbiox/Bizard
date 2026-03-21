# Skill: Barplot (errorbar2) (R)

## Category
Hiplot

## When to Use
Bar plot with error-lines and groups.

## Required R Packages
- data.table
- ggplot2
- ggpubr
- grafify
- jsonlite

## Minimal Reproducible Code
```r
# Load packages
library(data.table)
library(ggplot2)
library(ggpubr)
library(grafify)
library(jsonlite)

# Prepare data
# Load data
data <- data.table::fread(jsonlite::read_json("https://hiplot.cn/ui/basic/barplot-errorbar2/data.json")$exampleData[[1]]$textarea[[1]])
data <- as.data.frame(data)

# convert data structure
data[, 2] <- factor(data[, 2], levels = unique(data[, 2]))

# View data
head(data)

# Create visualization
# Barplot (errorbar2)
p <- plot_scatterbar_sd(
  data, ycol = get(colnames(data)[1]), xcol = get(colnames(data)[2]),
  b_alpha = 1, ewid = 0.2, jitter = 0.1) +
  stat_compare_means(data = data, aes(data[, 2], data[, 1], fill = data[, 2]),
                     label = "p.format", ref.group = ".all.", vjust = -2, 
                     method = "t.test") +
  guides(fill=guide_legend(title=colnames(data)[2])) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.2))) +
  labs(x="class", y="score") +
  scale_fill_manual(values = c("#E64B35FF","#4DBBD5FF","#00A087FF","#3C5488FF")) +
  theme_classic2() +
  theme(text = element_text(family = "Arial"),
        plot.title = element_text(size = 12,hjust = 0.5),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 10),
        axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 1),
        legend.position = "right",
        legend.direction = "vertical",
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 10))

p
```

## Key Parameters
- `fill`: Maps `data` to the fill aesthetic
- `alpha`: Controls transparency (0 = fully transparent, 1 = opaque)
- `position`: Position adjustment (identity, dodge, stack, fill)
- `stat`: Statistical transformation to use
- `theme`: Plot theme; tutorial uses `theme_classic2()`

## Tips
- Customize color scales with `scale_fill_manual()` or `scale_color_brewer()`
- Adjust text size with `theme(text = element_text(size = 14))` for presentations
- See the full tutorial for additional customization options and advanced examples

## Full Tutorial
https://openbiox.github.io/Bizard/Hiplot/006-barplot-errorbar2.html
