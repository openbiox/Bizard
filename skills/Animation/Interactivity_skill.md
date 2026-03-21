# Skill: Interactivity (R)

## Category
Animation

## When to Use
Interactive charts allow users to perform actions: zoom, hover the mouse over markers for tooltips, select variables to display, and so on. R provides a set of packages called HTML widgets: these allow you to build interactive data visualizations directly from R.

## Required R Packages
- chorddiag
- d3heatmap
- dygraphs
- gapminder
- ggiraph
- htmlwidgets
- patchwork
- plotly
- streamgraph
- tidyverse
- webshot
- xts

## Minimal Reproducible Code
```r
# Load packages
library(chorddiag)
library(d3heatmap)
library(dygraphs)
library(gapminder)
library(ggiraph)
library(htmlwidgets)

# Prepare data
head(gapminder)

# Create visualization
plot1 <- gapminder %>%
  filter(year==1977) %>%
  ggplot(aes(gdpPercap, lifeExp, size = pop, color=continent))+
  geom_point() +
  theme_bw()
ggplotly(plot1)
```

## Key Parameters
- `size`: Maps `pop` to the size aesthetic
- `color`: Maps `continent` to the color aesthetic
- `x`: Maps `Sample` to the x aesthetic
- `y`: Maps `Composite` to the y aesthetic
- `fill`: Maps `Standardized_Level` to the fill aesthetic
- `width`: Controls element width
- `theme`: Plot theme; tutorial uses `theme_bw()`

## Tips
- Use `theme_minimal()` or `theme_bw()` for clean, publication-ready plots
- Use `coord_flip()` for horizontal orientation when labels are long
- See the full tutorial for additional customization options and advanced examples

## Full Tutorial
https://openbiox.github.io/Bizard/Animation/Interactivity.html
