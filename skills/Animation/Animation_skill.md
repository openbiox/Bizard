# Skill: Animation (R)

## Category
Animation

## When to Use
Create a Animation visualization in R for biomedical data analysis and research publications.

## Required R Packages
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

## Minimal Reproducible Code
```r
# Load packages
library(Cairo)
library(babynames)
library(dplyr)
library(gapminder)
library(gganimate)
library(ggplot2)

# Prepare data
# data_gapminder
data_gapminder <- gapminder

# data_babynames
data_babynames <- babynames

# data_covi19
data_covi19 <- readr::read_csv("https://bizard-1301043367.cos.ap-guangzhou.myqcloud.com/data_covi19.csv")
data_covi19$date <- c(6:12)
data_covi19 <- gather(data_covi19, key = "area", value = "cases", 2:7)

# data_covi19_bar
data_covi19_bar_10 <- filter(data_covi19, date == 10)
data_covi19_bar_10$frame <- rep("a",6)

data_covi19_bar_12 <- filter(data_covi19, date == 12)
data_covi19_bar_12$frame <- rep("b",6)

data_covi19_bar <- rbind(data_covi19_bar_10,data_covi19_bar_12)

# Create visualization
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

## Key Parameters
- `size`: Maps `pop` to the size aesthetic
- `color`: Maps `continent` to the color aesthetic
- `colour`: Maps `country` to the colour aesthetic
- `x`: Maps `group` to the x aesthetic
- `y`: Maps `values` to the y aesthetic
- `fill`: Maps `group` to the fill aesthetic
- `alpha`: Controls transparency (0 = fully transparent, 1 = opaque)
- `stat`: Statistical transformation to use

## Tips
- Use `theme_minimal()` or `theme_bw()` for clean, publication-ready plots
- Customize color scales with `scale_fill_manual()` or `scale_color_brewer()`
- Use `facet_wrap()` or `facet_grid()` to create multi-panel plots by group

## Full Tutorial
https://openbiox.github.io/Bizard/Animation/Animation.html
