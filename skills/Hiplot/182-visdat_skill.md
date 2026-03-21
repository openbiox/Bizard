# Skill: Visdat (R)

## Category
Hiplot

## When to Use
Create a Visdat using R with the Hiplot platform's approach. Suitable for biomedical data visualization with publication-quality output.

## Required R Packages
- data.table
- dplyr
- ggplot2
- jsonlite
- patchwork
- visdat

## Minimal Reproducible Code
```r
# Load packages
library(data.table)
library(dplyr)
library(ggplot2)
library(jsonlite)
library(patchwork)
library(visdat)

# Prepare data
# Load data
data <- data.table::fread(jsonlite::read_json("https://hiplot.cn/ui/basic/visdat/data.json")$exampleData$textarea[[1]])
data <- as.data.frame(data)

# View data
head(data)

# Create visualization
# Visdat
add_palette <- function (p) {
  ## add color palette
  p <- p + scale_fill_manual(values = c("#3B4992FF", "#EE0000FF"))
}
pobj <- list()
pobj[["p1"]] <- add_palette(vis_dat(data)) + ggtitle(':vis_dat')
pobj[["p2"]] <- add_palette(vis_guess(data)) + ggtitle(':vis_guess')
pobj[["p3"]] <- vis_miss(data, cluster = T, sort_miss = T) + ggtitle(':vis_miss')
pobj[["p4"]] <- add_palette(vis_expect(data, ~.x >= 20 )) + ggtitle(':vis_expect')
pobj[["p5"]] <- vis_cor(data) + 
  scale_fill_gradientn(colours = c("#0571B0", "#92C5DE", "#F4A582", "#CA0020")) +
  ggtitle(':vis_cor')
pobj[["p6"]] <- data %>%
      select_if(is.numeric) %>%
      vis_value() + ggtitle(':vis_value')
pobj[["p6"]] <- pobj[["p6"]] + 
  scale_fill_gradientn(colours = c("#0571B0","#92C5DE","#F7F7F7","#F4A582",
                                   "#CA0020"))

pstr <- paste0(sprintf("pobj[[%s]]", 1:length(pobj)), collapse = " + ")
p <- eval(parse(text = 
  sprintf("%s + plot_layout(ncol = 2) +
plot_annotation(tag_levels = 'A')", pstr)))

p
```

## Key Parameters
- `fill`: Maps a variable to fill color for group comparison
- `color`: Maps a variable to outline/point color

## Tips
- Customize color scales with `scale_fill_manual()` or `scale_color_brewer()`
- Adjust text size with `theme(text = element_text(size = 14))` for presentations
- See the full tutorial for additional customization options and advanced examples

## Full Tutorial
https://openbiox.github.io/Bizard/Hiplot/182-visdat.html
