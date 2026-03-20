# Skill: Visdat (R)

## Category
Hiplot

## When to use
::: callout-note
**Hiplot website**

## Required R packages
- data.table
- dplyr
- ggplot2
- jsonlite
- patchwork
- visdat

## Minimal reproducible code
```r
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

## Full tutorial
https://openbiox.github.io/Bizard/Hiplot/182-visdat.html
