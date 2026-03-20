# Skill: Heatmap (R)

## Category
Hiplot

## When to use
::: callout-note
**Hiplot website**

## Required R packages
- ComplexHeatmap
- data.table
- genefilter
- jsonlite

## Minimal reproducible code
```r
# Heatmap
## Set annotation_col and annotation_row to add annotations to samples and genes respectively
top_var <- 100
top_var_genes <- rownames(data)[head(
  order(genefilter::rowVars(data), decreasing = TRUE),
  nrow(data) * top_var / 100
)]
## Set annotation_colors
col <- colorRampPalette(c("#0060BF","#FFFFFF","#CA1111"))(50)
annotation_colors <- list()
for(i in colnames(sample_info_reorder)) {
  if (is.numeric(sample_info_reorder[,i])) {
    annotation_colors[[i]] <- col
  } else {
    ref <- c("#323232","#1B6393")
    annotation_colors[[i]] <- ref
    names(annotation_colors[[i]]) <- unique(sample_info_reorder[,i])
  }
}
for(i in colnames(gene_info_reorder)) {
  if (is.numeric(gene_info_reorder[,i])) {
    annotation_colors[[i]] <- col
  } else {
    ref <- c("#323232","#1B6393")
    annotation_colors[[i]] <- ref
    names(annotation_colors[[i]]) <- unique(gene_info_reorder[,i])
  }
}

p <- 
  ComplexHeatmap::pheatmap(
    data[row.names(data) %in% top_var_genes,],
    color = col, 
    border_color = NA,
    fontsize_row = 6, fontsize_col = 6,
    main = "Heatmap Plot",
    cluster_rows = T, cluster_cols = T,
    scale = "none",
    clustering_method = "ward.D2",
    clustering_distance_cols = "euclidean",
    clustering_distance_rows = "euclidean",
    fontfamily = "Arial",
    display_numbers = F,
    number_color = "black",
    annotation_col = sample_info_reorder,
    annotation_row = gene_info_reorder,
    annotation_colors = annotation_colors
  )

p
```

## Full tutorial
https://openbiox.github.io/Bizard/Hiplot/086-heatmap.html
