# Skill: Half Violin (R)

## Category
Hiplot

## When to Use
The half violin plot is a statistical graph used to display the distribution and probability density of data by replacing the left part with the data frequency count graph on the basis of keeping the right part of violin graph.

## Required R Packages
- data.table
- dplyr
- ggplot2
- ggpubr
- ggthemes
- jsonlite

## Minimal Reproducible Code
```r
# Load packages
library(data.table)
library(dplyr)
library(ggplot2)
library(ggpubr)
library(ggthemes)
library(jsonlite)

# Prepare data
# Load data
data <- data.table::fread(jsonlite::read_json("https://hiplot.cn/ui/basic/half-violin/data.json")$exampleData$textarea[[1]])
data <- as.data.frame(data)

# Convert data structure
colnames(data) <- c("Value", "Group")
data[, 2] <- factor(data[, 2], levels = unique(data[, 2]))

# View data
head(data)

# Create visualization
# Half Violin
geom_flat_violin <- function(
  mapping = NULL, data = NULL, stat = "ydensity", position = "dodge", 
  trim = TRUE, scale = "area", show.legend = NA, inherit.aes = TRUE, ...) {
  ggplot2::layer(data = data, mapping = mapping, stat = stat, 
                 geom = geom_flat_violin_proto, position = position,
                 show.legend = show.legend, inherit.aes = inherit.aes,
                 params = list(trim = trim, scale = scale, ...))
}

"%||%" <- function(a, b) {
  if (!is.null(a)) {
    a
  } else {
    b
  }
}

geom_flat_violin_proto <-
  ggproto("geom_flat_violin_proto", Geom,
          setup_data = function(data, params) {
            data$width <- data$width %||%
              params$width %||% (resolution(data$x, FALSE) * 0.9)
            
            data %>%
              dplyr::group_by(.data = ., group) %>%
              dplyr::mutate(.data = ., ymin = min(y), ymax = max(y), xmin = x,
                            xmax = x + width / 2)
          },
# ... (see full tutorial for more)
```

## Key Parameters
- `size`: Maps `0` to the size aesthetic
- `alpha`: Maps `NA` to the alpha aesthetic
- `fill`: Maps `Group` to the fill aesthetic
- `width`: Controls element width
- `position`: Position adjustment (identity, dodge, stack, fill)
- `stat`: Statistical transformation to use
- `theme`: Plot theme; tutorial uses `theme_stata()`

## Tips
- Customize color scales with `scale_fill_manual()` or `scale_color_brewer()`
- Adjust text size with `theme(text = element_text(size = 14))` for presentations
- See the full tutorial for additional customization options and advanced examples

## Full Tutorial
https://openbiox.github.io/Bizard/Hiplot/085-half-violin.html
