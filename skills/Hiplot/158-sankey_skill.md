# Skill: Sankey (R)

## Category
Hiplot

## When to Use
Sankey diagrams are a type of flow diagramin which the width of the arrows is proportional to the flow rate.

## Required R Packages
- data.table
- ggalluvial
- ggplot2
- jsonlite

## Minimal Reproducible Code
```r
# Load packages
library(data.table)
library(ggalluvial)
library(ggplot2)
library(jsonlite)

# Prepare data
# Load data
data <- data.table::fread(jsonlite::read_json("https://hiplot.cn/ui/basic/sankey/data.json")$exampleData$textarea[[1]])
data <- as.data.frame(data)

# Convert data structure
value <- "Freq"
axis <- c("Class", "Sex")
usr_axis <- c()
for (i in seq_len(length(axis))) {
  usr_axis <- c(usr_axis, axis[i])
  assign(paste0("axis", i), axis[i])
}
index_axis <- match(usr_axis, colnames(data))
index_value <- match(value, colnames(data))
data1 <- data[, c(index_value, index_axis)]
## define band color
nlevels <- as.numeric(apply(data1[, -1], 2, function(data) {
  return(length(unique(data)))
}))
band_color <- c("#8DD3C7", "#FFFFB3", "#BEBADA", "#FB8072", "#8DD3C7", "#FFFFB3")
## rename data
data_rename <- data1
colnames(data_rename) <- c(
  "value",
  paste("axis", seq_len(length(usr_axis)), sep = "")
)

# View data
head(data)

# Create visualization
# Sankey
p <- ggplot(data_rename, aes(y = value, axis1 = axis1, axis2 = axis2)) +
  geom_alluvium(alpha = 1, aes(fill = data1[, colnames(data1) == "Sex"]),
                width = 0, reverse = FALSE) +
  scale_x_discrete(limits = usr_axis, expand = c(0.02, 0.1)) +
  ylab("") +
  scale_fill_discrete(name = "Sex") +
  coord_flip() +
  geom_stratum(alpha = 1, width = 1 / 8, reverse = FALSE, fill = band_color,
               color = "white") +
  geom_text(stat = "stratum", infer.label = TRUE, reverse = FALSE) +
  ggtitle("Sankey plot") +
# ... (see full tutorial for more)
```

## Key Parameters
- `y`: Maps `value` to the y aesthetic
- `fill`: Maps `data1` to the fill aesthetic
- `alpha`: Controls transparency (0 = fully transparent, 1 = opaque)
- `width`: Controls element width
- `position`: Position adjustment (identity, dodge, stack, fill)
- `stat`: Statistical transformation to use
- `theme`: Plot theme; tutorial uses `theme_bw()`

## Tips
- Use `theme_minimal()` or `theme_bw()` for clean, publication-ready plots
- Use `coord_flip()` for horizontal orientation when labels are long
- Customize color scales with `scale_fill_manual()` or `scale_color_brewer()`

## Full Tutorial
https://openbiox.github.io/Bizard/Hiplot/158-sankey.html
