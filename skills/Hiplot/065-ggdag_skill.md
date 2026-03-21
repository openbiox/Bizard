# Skill: Directed Acyclic Graphs (R)

## Category
Hiplot

## When to Use
Visualizing directed acyclic graphs.

## Required R Packages
- ggdag

## Minimal Reproducible Code
```r
# Load packages
library(ggdag)

# Prepare data
# Load data
tidy_ggdag <- dagify(
  y ~ x + z2 + w2 + w1,
  x ~ z1 + w1 + w2,
  z1 ~ w1 + v,
  z2 ~ w2 + v,
  w1 ~ ~w2, # bidirected path
  exposure = "x",
  outcome = "y") %>%
  tidy_dagitty()

# View data
head(tidy_ggdag)

# Create visualization
# Directed Acyclic Graphs
p <- ggdag(tidy_ggdag) +
  theme_dag() 

p
```

## Key Parameters
- `theme`: Plot theme; tutorial uses `theme_dag()`
- `fill`: Maps a variable to fill color for group comparison
- `color`: Maps a variable to outline/point color

## Tips
- Adjust text size with `theme(text = element_text(size = 14))` for presentations
- See the full tutorial for additional customization options and advanced examples

## Full Tutorial
https://openbiox.github.io/Bizard/Hiplot/065-ggdag.html
