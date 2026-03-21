# Skill: Connected Scatterplot (R)

## Category
Hiplot

## When to Use
Connected scatterplot

## Required R Packages
- data.table
- dplyr
- ggplot2
- ggrepel
- jsonlite

## Minimal Reproducible Code
```r
# Load packages
library(data.table)
library(dplyr)
library(ggplot2)
library(ggrepel)
library(jsonlite)

# Prepare data
# Load data
data <- data.table::fread(jsonlite::read_json("https://hiplot.cn/ui/basic/connected-scatterplot/data.json")$exampleData$textarea[[1]])
data <- as.data.frame(data)

# View data
head(data)

# Create visualization
# Connected Scatterplot
connected_scatterplot <- function(data, x, y, label, label_ratio, line_color, arrow_size, label_size) {

  draw_data <- data.frame(
    x = data[[x]],
    y = data[[y]],
    label = data[[label]]
  )

  add_label_data <- draw_data %>% sample_frac(label_ratio)
  rm(data)

  p <- ggplot(draw_data, aes(x = x, y = y, label = label)) +
    geom_point(color = line_color) +
    geom_text_repel(data = add_label_data, size = label_size) +
    geom_segment(
      color = line_color,
      aes(
        xend = c(tail(x, n = -1), NA),
        yend = c(tail(y, n = -1), NA)
      ),
      arrow = arrow(length = unit(arrow_size, "mm"))
    )

  return(p)
}

p <- connected_scatterplot(
  data = if (exists("data") && is.data.frame(data)) data else "",
  x = "Alice",
  y = "Anna",
  label = "year",
  label_ratio = 0.5,
  line_color = "#1A237E",
# ... (see full tutorial for more)
```

## Key Parameters
- `x`: Maps `x` to the x aesthetic
- `y`: Maps `y` to the y aesthetic
- `position`: Position adjustment (identity, dodge, stack, fill)
- `theme`: Plot theme; tutorial uses `theme_bw()`

## Tips
- Use `theme_minimal()` or `theme_bw()` for clean, publication-ready plots
- Adjust text size with `theme(text = element_text(size = 14))` for presentations
- See the full tutorial for additional customization options and advanced examples

## Full Tutorial
https://openbiox.github.io/Bizard/Hiplot/026-connected-scatterplot.html
