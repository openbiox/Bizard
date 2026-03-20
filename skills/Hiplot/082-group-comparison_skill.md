# Skill: Group-comparison Heatmap (R)

## Category
Hiplot

## When to use
::: callout-note
**Hiplot website**

## Required R packages
- data.table
- jsonlite
- sigminer

## Minimal reproducible code
```r
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
      cluster_row = cluster_row
    )
  }
  return(p)
}

# plot
p <- plotentry(
  data = data,
  grp_vars = "g1",
  enrich_vars = c("e1", "e2"),
  cross = T,
  add_text_annotation = T,
  fill_by_p_value = T,
  use_fdr = T,
  cut_p_value = F,
  cluster_row = F,
  co_method = "t.test",
  scales = "free"
)

p
```

## Full tutorial
https://openbiox.github.io/Bizard/Hiplot/082-group-comparison.html
