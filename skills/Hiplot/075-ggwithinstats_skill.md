# Skill: Complex-Violin (R)

## Category
Hiplot

## When to Use
Create a Complex-Violin using R with the Hiplot platform's approach. Suitable for biomedical data visualization with publication-quality output.

## Required R Packages
- cowplot
- data.table
- ggplot2
- ggstatsplot
- jsonlite

## Minimal Reproducible Code
```r
# Load packages
library(cowplot)
library(data.table)
library(ggplot2)
library(ggstatsplot)
library(jsonlite)

# Prepare data
# Load data
data <- data.table::fread(jsonlite::read_json("https://hiplot.cn/ui/basic/ggwithinstats/data.json")$exampleData$textarea[[1]])
data <- as.data.frame(data)

# Convert data structure
axis <- c("condition", "desire", "region")
data[, axis[1]] <- factor(data[, axis[1]], levels = unique(data[, axis[1]]))
data[, axis[3]] <- factor(data[, axis[3]], levels = unique(data[, axis[3]]))

# View data
str(data)

# Create visualization
# Complex-Violin
g <- unique(data[,axis[3]])
plist <- list()
for (i in 1:length(g)) {
  fil <- data[,axis[3]] == g[i]
  plist[[i]] <- ggwithinstats(
    data = data[fil,], x = condition, y = desire,
    title= paste('', axis[3], g[i], sep = ':'),
    p.adjust.method = "holm",
    plot.type = "boxviolin",
    pairwise.comparisons = T,
    pairwise.display = "significant",
    effsize.type = "unbiased",
    notch = T,
    type = "parametric",
    k = 2,
    plotgrid.args = list(ncol = 2)
  ) +
    scale_color_manual(values = c("#3B4992FF","#EE0000FF"))
}

plot_grid(plotlist = plist, ncol = 2)
```

## Key Parameters
- `stat`: Statistical transformation to use
- `fill`: Maps a variable to fill color for group comparison
- `color`: Maps a variable to outline/point color

## Tips
- Customize color scales with `scale_fill_manual()` or `scale_color_brewer()`
- Adjust text size with `theme(text = element_text(size = 14))` for presentations
- See the full tutorial for additional customization options and advanced examples

## Full Tutorial
https://openbiox.github.io/Bizard/Hiplot/075-ggwithinstats.html
