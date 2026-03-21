# Skill: Treeheatr (R)

## Category
Hiplot

## When to Use
The heatmap decision tree is a visualization graph that combines two types of graphs: heatmap and decision tree visualization.

## Required R Packages
- data.table
- ggplotify
- jsonlite
- treeheatr

## Minimal Reproducible Code
```r
# Load packages
library(data.table)
library(ggplotify)
library(jsonlite)
library(treeheatr)

# Prepare data
# Load data
data <- data.table::fread(jsonlite::read_json("https://hiplot.cn/ui/basic/treeheatr/data.json")$exampleData$textarea[[1]])
data <- as.data.frame(data)

# convert data structure
x <- data
wrong_cols <- suppressWarnings(sapply(x, function(x) {
  if (!is.numeric(x)) {
    sum(!is.na(as.numeric(x))) > 0.7 * length(x)
  } else {
    FALSE
  }
}))
if (any(wrong_cols)) {
  ix <- which(wrong_cols)
  for (i in ix) {
    data[[i]] <- suppressWarnings(as.numeric(data[[i]]))
  }
  rm(ix)
}
rm(x, wrong_cols)

# View data
head(data)

# Create visualization
# Treeheatr
p <- as.ggplot(function() {
  print(heat_tree(data,
    target_lab = "species",
    task = 'classification',
    show = "heat-tree",
    heat_rel_height = 0.2,
    panel_space = 0.001,
    clust_samps = T,
    clust_target = T,
    lev_fac = 1.3,
    cont_legend = F,
    cate_legend = F
  ))
})

p
```

## Key Parameters
- `fill`: Maps a variable to fill color for group comparison
- `color`: Maps a variable to outline/point color

## Tips
- Adjust text size with `theme(text = element_text(size = 14))` for presentations
- See the full tutorial for additional customization options and advanced examples

## Full Tutorial
https://openbiox.github.io/Bizard/Hiplot/172-treeheatr.html
