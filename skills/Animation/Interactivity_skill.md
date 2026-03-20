# Skill: Interactivity (R)

## Category
Animation

## When to use
Interactive charts allow users to perform actions: zoom, hover the mouse over markers for tooltips, select variables to display, and so on. R provides a set of packages called HTML widgets: these allow you to build interactive data visualizations directly from R.

## Required R packages
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

## Minimal reproducible code
```r
plot1 <- gapminder %>%
  filter(year==1977) %>%
  ggplot(aes(gdpPercap, lifeExp, size = pop, color=continent))+
  geom_point() +
  theme_bw()
ggplotly(plot1)
```

## Full tutorial
https://openbiox.github.io/Bizard/Animation/Interactivity.html
