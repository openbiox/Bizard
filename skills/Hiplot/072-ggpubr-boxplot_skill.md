# Skill: GGPubr Boxplot (R)

## Category
Hiplot

## When to Use
Feature-rich boxplot (GGPubr interface).

## Required R Packages
- data.table
- ggpubr
- ggthemes
- jsonlite

## Minimal Reproducible Code
```r
# Load packages
library(data.table)
library(ggpubr)
library(ggthemes)
library(jsonlite)

# Prepare data
# Load data
data <- data.table::fread(jsonlite::read_json("https://hiplot.cn/ui/basic/ggpubr-boxplot/data.json")$exampleData$textarea[[1]])
data <- as.data.frame(data)

# View data
head(data)

# Create visualization
# GGPubr Boxplot
p <- ggboxplot(
  data = data, x = "supp", y = "len", facet.by = "dose",
  merge = T,
  color = "supp",
  fill = "white") + 
  stat_compare_means(
    label = "p.signif",
    label.x.npc = "center",
    method = "wilcox") + 
  scale_y_continuous(expand = expansion(mult = c(0.2, 0.2))) +
  scale_fill_manual(values = c("#e04d39","#5bbad6")) +
  ggtitle("Complex Boxplot") + 
  theme_stata() +
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
- `position`: Position adjustment (identity, dodge, stack, fill)
- `stat`: Statistical transformation to use
- `theme`: Plot theme; tutorial uses `theme_stata()`

## Tips
- Customize color scales with `scale_fill_manual()` or `scale_color_brewer()`
- Adjust text size with `theme(text = element_text(size = 14))` for presentations
- See the full tutorial for additional customization options and advanced examples

## Full Tutorial
https://openbiox.github.io/Bizard/Hiplot/072-ggpubr-boxplot.html
