# Skill: Piestats Group (R)

## Category
Hiplot

## When to Use
Create a Piestats Group using R with the Hiplot platform's approach. Suitable for biomedical data visualization with publication-quality output.

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
data <- data.table::fread(jsonlite::read_json("https://hiplot.cn/ui/basic/ggpiestats-group/data.json")$exampleData$textarea[[1]])
data <- as.data.frame(data)

# Convert data structure
axis <- c("genre", "mpaa")
data[, axis[1]] <- factor(data[, axis[1]], levels = unique(data[, axis[1]]))
data[, axis[2]] <- factor(data[, axis[2]], levels = unique(data[, axis[2]]))

# View data
head(data)

# Create visualization
# Piestats Group
g <- unique(data[,axis[2]])
plist <- list()
for (i in 1:length(g)) {
  fil <- data[,axis[2]] == g[i]
  plist[[i]] <- 
    ggpiestats(
      data = data[fil,], x = genre, 
      title= paste('', axis[2], g[i], sep = ':'),
      plotgrid.args = list(ncol = 3),
      label.repel = TRUE,
      k = 2
    ) +
    scale_fill_manual(values = c("#3B4992FF","#EE0000FF","#008B45FF","#631879FF",
                                 "#008280FF","#BB0021FF","#5F559BFF","#A20056FF",
                                 "#808180FF"))
}

plot_grid(plotlist = plist, ncol = 3)
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
https://openbiox.github.io/Bizard/Hiplot/069-ggpiestats-group.html
