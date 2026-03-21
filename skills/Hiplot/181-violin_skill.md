# Skill: Violin (R)

## Category
Hiplot

## When to Use
The violin plot, named for its resemblance to a violin, is a statistical diagram combining a box diagram with a kernel density diagram to show the distribution of data and the probability density.

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
data <- data.table::fread(jsonlite::read_json("https://hiplot.cn/ui/basic/violin/data.json")$exampleData$textarea[[1]])
data <- as.data.frame(data)

# convert data structure
groups <- unique(data[, 2])
ngroups <- length(groups)
comb <- combn(1:ngroups, 2)
my_comparisons <- list()
for (i in seq_len(ncol(comb))) {
  my_comparisons[[i]] <- groups[comb[, i]]
}

# View data
head(data)

# Create visualization
# Violin
p <- ggviolin(data, x = "Tumor", y = "Expresssion", fill = "Tumor", add = "boxplot",
              xlab = "Tumor", ylab = "Expresssion", 
              add.params = list(fill = "white"),
              palette = c("#e04d39","#5bbad6","#1e9f86"),
              title = "Violin Plot", alpha = 1) + 
  stat_compare_means(comparisons = my_comparisons, label = "p.signif") +
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
- `alpha`: Controls transparency (0 = fully transparent, 1 = opaque)
- `position`: Position adjustment (identity, dodge, stack, fill)
- `stat`: Statistical transformation to use
- `theme`: Plot theme; tutorial uses `theme_stata()`

## Tips
- Adjust text size with `theme(text = element_text(size = 14))` for presentations
- See the full tutorial for additional customization options and advanced examples

## Full Tutorial
https://openbiox.github.io/Bizard/Hiplot/181-violin.html
