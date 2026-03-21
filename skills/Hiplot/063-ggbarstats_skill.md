# Skill: Barstats (R)

## Category
Hiplot

## When to Use
Create a Barstats using R with the Hiplot platform's approach. Suitable for biomedical data visualization with publication-quality output.

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
data <- data.table::fread(jsonlite::read_json("https://hiplot.cn/ui/basic/ggbarstats/data.json")$exampleData$textarea[[1]])
data <- as.data.frame(data)

# Convert data structure
axis <- c("relig", "partyid", "race")
data[, axis[1]] <- factor(data[, axis[1]], levels = rev(unique(data[, axis[1]])))
data[, axis[2]] <- factor(data[, axis[2]], levels = unique(data[, axis[2]]))
data[, axis[3]] <- factor(data[, axis[3]], levels = unique(data[, axis[3]]))

# View data
head(data)

# Create visualization
# Barstats
g <- unique(data[,axis[3]])
plist <- list()
for (i in 1:length(g)) {
  fil <- data[,axis[3]] == g[i]
  plist[[i]] <- ggbarstats(
    data = data[fil,], x = relig, y = partyid,
    plotgrid.args = list(ncol = 1), paired = F, k = 2) +
    scale_fill_manual(values = c("#00468BFF","#ED0000FF","#42B540FF"))
}
p <- plot_grid(plotlist = plist, ncol = 1)

p
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
https://openbiox.github.io/Bizard/Hiplot/063-ggbarstats.html
