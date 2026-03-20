# Skill: Upset Plot (R)

## Category
Hiplot

## When to use
::: callout-note
**Hiplot website**

## Required R packages
- ComplexHeatmap
- VennDiagram
- data.table
- ggplot2
- ggplotify
- jsonlite

## Minimal reproducible code
```r
# Upset Plot
p <- as.ggplot(function(){
  top_annotation <- HeatmapAnnotation(
    Intersections = anno_barplot(
      cs, ylim = c(0, max(cs)*1.1), 
      border = FALSE, 
      gp = gpar(fill = "#000000", fontsize = 10), 
      height = unit(5, "cm")
    ), 
    annotation_name_side = "left", 
    annotation_name_rot = 90
  )
  
  left_annotation <- rowAnnotation(
    Numbers = anno_barplot(-ss, axis_param = list(
      at = seq(-max(ss), 0, round(max(ss)/5)),
      labels = rev(seq(0, max(ss), round(max(ss)/5))),
      labels_rot = 0),
      baseline = 0,
      border = FALSE, 
      gp = gpar(fill = "#000000", fontsize = 10), 
      width = unit(4, "cm")
    ),
    set_name = anno_text(set_name(m), location = 0.5,  just = "center",
                         width = max_text_width(set_name(m)) + unit(5, "mm"))
  )
  
  ht = UpSet(m, comb_col = "#000000", bg_col = "#F0F0F0", bg_pt_col = "#CCCCCC",
             pt_size = unit(3, "mm"), lwd = 2, set_order = set_order,
             comb_order = comb_order, top_annotation = top_annotation,
             left_annotation = left_annotation,  right_annotation = NULL,
             show_row_names = FALSE)
  ht = draw(ht)
  od = column_order(ht)
  decorate_annotation("Intersections", {
    grid.text(cs[od], x = seq_along(cs), y = unit(cs[od], "native") + unit(2, "pt"),
              default.units = "native", just = c("left", "bottom"), 
              gp = gpar(fontsize = 10, col = "#000000",
              fontfamily = "Arial"), hjust = 0.5)
  })
})
p <- p + ggtitle("Upset Plot") + 
  theme(plot.title = element_text(hjust = 0.6))

p
```

## Full tutorial
https://openbiox.github.io/Bizard/Hiplot/177-upset-plot.html
