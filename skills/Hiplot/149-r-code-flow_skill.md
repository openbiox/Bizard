# Skill: R Script Flow (R)

## Category
Hiplot

## When to Use
R script flow can realize the visual window of if, else and other logic functions.

## Required R Packages
- flow

## Minimal Reproducible Code
```r
# Load packages
library(flow)

# Prepare data
# Load data
code <- function(){
  if (x < 10) {
    a <- 1
  } else {
    a <- 2
  }
  if (a == 2) {
    c <- d
  } else {
    d <- a
  }
}

# Create visualization
# R Script Flow
p <- flow_view(code)

p
```

## Key Parameters
- `fill`: Maps a variable to fill color for group comparison
- `color`: Maps a variable to outline/point color

## Tips
- Adjust text size with `theme(text = element_text(size = 14))` for presentations
- See the full tutorial for additional customization options and advanced examples

## Full Tutorial
https://openbiox.github.io/Bizard/Hiplot/149-r-code-flow.html
