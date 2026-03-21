# Skill: Gene Density (R)

## Category
Hiplot

## When to Use
Chrosome data visualization.

## Required R Packages
- ComplexHeatmap
- RColorBrewer
- circlize
- data.table
- ggplotify
- gtrellis
- jsonlite
- tidyverse

## Minimal Reproducible Code
```r
# Load packages
library(ComplexHeatmap)
library(RColorBrewer)
library(circlize)
library(data.table)
library(ggplotify)
library(gtrellis)

# Prepare data
# Load data
data1 <- data.table::fread(jsonlite::read_json("https://hiplot.cn/ui/basic/gene-density/data.json")$exampleData$textarea[[1]])
data1 <- as.data.frame(data1)
data2 <- data.table::fread(jsonlite::read_json("https://hiplot.cn/ui/basic/gene-density/data.json")$exampleData$textarea[[2]])
data2 <- as.data.frame(data2)

# Convert data structure
chrNum <- str_replace(unique(data1$chr), "Chr|chr", "")
data1$chr <- factor(data1$chr, levels = paste0("Chr", chrNum))
data2$chr <- factor(data2$chr, levels = paste0("Chr", chrNum))
# Set window to calculate gene density
windows <- 100 * 1000 # default:100kb window size
gene_density <- genomicDensity(data2, window.size = windows)
gene_density$chr <- factor(gene_density$chr,
  levels =  paste0("Chr", chrNum)
)

# View data
head(data1)
head(data2)

# Create visualization
# Set the palettes
palettes <- c("#B2182B","#EF8A62","#FDDBC7","#D1E5F0","#67A9CF","#2166AC")
col_fun <- colorRamp2(
  seq(0, max(gene_density[[4]]), length = 6), rev(palettes)
  )
cm <- ColorMapping(col_fun = col_fun)
# Set the Legend
lgd <- color_mapping_legend(
  cm, plot = F, title = "density", color_bar = "continuous"
  )
# Plot
p <- as.ggplot(function() {
  gtrellis_layout(
    data1, n_track = 2, ncol = 1, byrow = FALSE,
    track_axis = FALSE, add_name_track = FALSE,
    xpadding = c(0.1, 0), gap = unit(1, "mm"),
    track_height = unit.c(unit(1, "null"), unit(4, "mm")),
    track_ylim = c(0, max(gene_density[[4]]), 0, 1),
    border = FALSE, asist_ticks = FALSE,
# ... (see full tutorial for more)
```

## Key Parameters
- `fill`: Maps a variable to fill color for group comparison
- `color`: Maps a variable to outline/point color

## Tips
- Adjust text size with `theme(text = element_text(size = 14))` for presentations
- See the full tutorial for additional customization options and advanced examples

## Full Tutorial
https://openbiox.github.io/Bizard/Hiplot/060-gene-density.html
