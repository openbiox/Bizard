# Skill: Group-comparison Heatmap (R)

## Category
Hiplot

## When to Use
Group-comparison Heatmap provides a way to compare multiple variables across multiple (>2) groups and visualize the result with heatmap.

## Required R Packages
- data.table
- jsonlite
- sigminer

## Minimal Reproducible Code
```r
# Load packages
library(data.table)
library(jsonlite)
library(sigminer)

# Prepare data
# Load data
data <- data.table::fread(jsonlite::read_json("https://hiplot.cn/ui/basic/group-comparison/data.json")$exampleData$textarea[[1]])
data <- as.data.frame(data)

# View data
head(data)

# Create visualization
# Define plot functions
unlist_and_covert <- function(x, recursive = FALSE) {
  if (!is.null(x)) {
    x <- unlist(x, recursive = recursive)
    if (!is.null(x)) {
      y <- sapply(x, function(x) {
        if (identical(x, "NA")) NA else x
      })
      names(y) <- names(x)
      x <- y
    }
  }
  x
}

plotentry <- function(data,
                      grp_vars = NULL, enrich_vars = NULL, cross = TRUE,
                      co_method = c("t.test", "wilcox.test"), ref_group = NA,
                      scales = "free", add_text_annotation = TRUE,
                      fill_by_p_value = TRUE, use_fdr = TRUE, cut_p_value = FALSE,
                      cluster_row = FALSE) {
  ref_group <- unlist_and_covert(ref_group)
  if (is.null(ref_group)) ref_group <- NA
  rv <- group_enrichment(data, grp_vars, enrich_vars, cross, co_method, ref_group)
  if (length(unique(rv$grp_var)) == 1) {
    p <- show_group_enrichment(rv,
      return_list = TRUE,
      scales = scales, add_text_annotation = add_text_annotation,
      fill_by_p_value = fill_by_p_value, use_fdr = use_fdr, cut_p_value = cut_p_value,
      cluster_row = cluster_row
    )
    p <- p[[1]]
  } else {
    p <- show_group_enrichment(rv,
      scales = scales, add_text_annotation = add_text_annotation,
      fill_by_p_value = fill_by_p_value, use_fdr = use_fdr, cut_p_value = cut_p_value,
# ... (see full tutorial for more)
```

## Key Parameters
- `fill`: Maps a variable to fill color for group comparison
- `color`: Maps a variable to outline/point color

## Tips
- Adjust text size with `theme(text = element_text(size = 14))` for presentations
- See the full tutorial for additional customization options and advanced examples

## Full Tutorial
https://openbiox.github.io/Bizard/Hiplot/082-group-comparison.html
