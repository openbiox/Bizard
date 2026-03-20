# Skill: Gene Density (R)

## Category
Hiplot

## When to use
::: callout-note
**Hiplot website**

## Required R packages
- ComplexHeatmap
- RColorBrewer
- circlize
- data.table
- ggplotify
- gtrellis
- jsonlite
- tidyverse

## Minimal reproducible code
```r
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
    legend = lgd
    )
  # Add gene area map track
  add_lines_track(gene_density, gene_density[[4]],
                  area = TRUE, gp = gpar(fill = "pink"))
  # Add gene density heatmap track
  add_heatmap_track(gene_density, gene_density[[4]], fill = col_fun)
  add_track(track = 2, clip = FALSE, panel_fun = function(gr) {
    chr <- get_cell_meta_data("name")
    if (chr == paste("Chr", length(chrNum), sep = "")) {
      grid.lines(get_cell_meta_data("xlim"), unit(c(0, 0), "npc"),
                 default.units = "native")
      }
    grid.text(chr, x = 0.01, y = 0.38, just = c("left", "bottom"))
    })
  circos.clear()
  })

p
```

## Full tutorial
https://openbiox.github.io/Bizard/Hiplot/060-gene-density.html
