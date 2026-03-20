# Skill: Animation (R)

## Category
Animation

## When to use
Visualize animation data in a biomedical context.

## Required R packages
- Cairo
- babynames
- dplyr
- gapminder
- gganimate
- ggplot2
- gifski
- hrbrthemes
- tidyr
- viridis

## Minimal reproducible code
```r
# Scattered bubble animation----
p <- ggplot(data_gapminder, aes(gdpPercap, lifeExp, size = pop, color = continent)) +
  geom_point() +
  scale_x_log10() +
  theme_bw() +
  # Drawing animation
  labs(title = 'Year: {frame_time}', x = 'GDP per capita', y = 'life expectancy') +
  transition_time(year) +
  ease_aes('linear')

animate(p, renderer = gifski_renderer())
```

## Full tutorial
https://openbiox.github.io/Bizard/Animation/Animation.html
