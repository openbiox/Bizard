# Skill: Boxplot (R)

## Category
Hiplot

## When to Use
The box plot is a method of visualizing the distribution characteristics of a set of data by means of a quartile graph.

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
data <- data.table::fread(jsonlite::read_json("https://hiplot.cn/ui/basic/boxplot/data.json")$exampleData$textarea[[1]])
data <- as.data.frame(data)

# convert data structure
groups <- unique(data[, 2])
my_comparisons <- combn(groups, 2, simplify = FALSE)
my_comparisons <- lapply(my_comparisons, as.character)

# View data
head(data)

# Create visualization
# Boxplot
p <- ggboxplot(data, x = "Group1", y = "Value", notch = F, facet.by = "Group2",
               add = "point", color = "Group1", xlab = "Group2", ylab = "Value",
               palette = c("#e04d39","#5bbad6","#1e9f86"),
               title = "Box Plot") +
  stat_compare_means(comparisons = my_comparisons, label = "p.format", 
                     method = "t.test") +
  scale_y_continuous(expand = expansion(mult = c(0.1, 0.1))) +
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
- Adjust text size with `theme(text = element_text(size = 14))` for presentations
- See the full tutorial for additional customization options and advanced examples

## Full Tutorial
https://openbiox.github.io/Bizard/Hiplot/015-boxplot.html
