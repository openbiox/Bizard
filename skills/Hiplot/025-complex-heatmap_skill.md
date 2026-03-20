# Skill: Complex Heatmap (R)

## Category
Hiplot

## When to use
::: callout-note
**Hiplot website**

## Required R packages
- ComplexHeatmap
- circlize
- cowplot
- data.table
- ggplotify
- hiplotlib
- jsonlite
- randomcoloR
- stringr

## Minimal reproducible code
```r
# Complex Heatmap
params <- list()
for (i in names(col_meta)) {
  if (i != "Meta2") {
    params[[i]] <- data[, i]
  }
}
params2 <- list(
  Meta2 = meta_mat2,
  gap = 0,
  border = TRUE,
  show_annotation_name = TRUE,
  col = col_meta,
  na_col = "#FFFFFF",
  show_legend = FALSE,
  annotation_legend_param = list(direction = "horizontal")
)
for (i in names(params2)) {
  params[[i]] <- params2[[i]]
}
ha <- do.call(HeatmapAnnotation, params)
hlist <- Heatmap(heat_mat,
      col = hiplotlib::col_fun_cont(heat_mat, cols = color_key),
      name = "Expression",
      gap = 0,
      clustering_distance_columns = "euclidean",
      clustering_distance_rows = "euclidean",
      clustering_method_columns = "ward.D2",
      show_row_dend = TRUE, show_column_dend = TRUE,
      show_row_names = FALSE,
      row_title_gp = gpar(col = "#FFFFFF00"),
      cluster_rows = TRUE,
      cluster_columns = TRUE,
      bottom_annotation = ha,
      show_heatmap_legend = TRUE,
      heatmap_legend_param = list(direction = "horizontal")
    )
p1 <- as.ggplot(
  function() {
    draw(hlist, annotation_legend_side = "right", heatmap_legend_side = "top")
  }
)
idx <- sort(rowSums(!is.na(mut_mat) & mut_mat != "0" & mut_mat != ""), decreasing = TRUE)
mut_mat <- mut_mat[names(idx),]

p2 <- as.ggplot(
  function() {
    params <- list(
      mut_mat,
      get_type = function(x) strsplit(x, "/")[[1]],
      alter_fun = hiplotlib::alter_fun, col = cols, row_order = 1:nrow(mut_mat),
      show_column_names = TRUE,
      show_pct = TRUE,
      right_annotation = NULL,
      top_annotation = NULL,
      border = TRUE,
      heatmap_legend_param = list(direction = "horizontal"),
      show_heatmap_legend = FALSE)
      params$column_order <- unlist(column_order(hlist))
      draw(do.call(oncoPrint, params), annotation_legend_side = "bottom", heatmap_legend_side = "bottom")
    }
  )

p3 <- as.ggplot(function() {
    legend_tmp <- list()
    for (i in names(col_meta_pre)) {
      if (is.function(col_meta_pre[[i]])) {
        legend_tmp[[i]] <- Legend(
          col_fun = col_meta_pre[[i]],
          title = i, direction = "horizontal"
        )
      } else if (identical(col_meta_pre[[i]], c("#f4f4f4", "#5a5a5a"))) {
        legend_tmp[[i]] <- Legend(
          at = unique(data[, i]), title = i,
          direction = "horizontal",
          labels = c("No", "Yes"),
          legend_gp = gpar(fill = col_meta_pre[[i]])
        )
      } else {
         legend_tmp[[i]] <- Legend(
          at = unique(data[, i]), title = i,
          direction = "horizontal",
          legend_gp = gpar(fill = col_meta_pre[[i]])
        )
      }
    }
    ref_mut <- unique(unlist(str_split(mut_mat, "/")))
    ref_mut <- ref_mut[ref_mut != "" & ref_mut != "NANA"]
    ref_mut <- ref_mut[!is.na(ref_mut)]
    lgd_mut <- Legend(
      at = ref_mut, title = "Mutations",
      direction = "horizontal",
      legend_gp = gpar(fill = cols[ref_mut])
    )
    legend_tmp[[length(legend_tmp) + 1]] <- lgd_mut
    legend_tmp$direction <- "horizontal"
    legend_tmp$max_width <- unit(14, "cm")
    legend_tmp$column_gap <- unit(5, "mm")
    legend_tmp$row_gap <- unit(0.5, "cm")
    draw(do.call(packLegend, legend_tmp))
  })
rel_height <- as.numeric(str_split("4, 2, 2", ", |,| |;")[[1]])
p <- plot_grid(p1, p2, p3, ncol = 1, rel_heights = rel_height)
p
```

## Full tutorial
https://openbiox.github.io/Bizard/Hiplot/025-complex-heatmap.html
