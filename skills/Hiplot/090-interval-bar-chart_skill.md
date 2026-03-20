# Skill: Interval Bar Chart (R)

## Category
Hiplot

## When to use
::: callout-note
**Hiplot website**

## Required R packages
- data.table
- ggplot2
- jsonlite

## Minimal reproducible code
```r
# Interval Bar Chart
p <- ggplot(data, aes(x = month, y = max_temperature)) +
  geom_rect(aes(xmin = name_num - 0.4, xmax = name_num + 0.4,
                ymin = min_temperature, ymax = max_temperature), 
            fill = "#282726", alpha = 0.7) +
  geom_line(aes(x = name_num, y = mean), color = "#006064", size = 0.8) +
  labs(x = "Month", y = "Temperature") +
  scale_x_discrete() +
  theme_bw()

p
```

## Full tutorial
https://openbiox.github.io/Bizard/Hiplot/090-interval-bar-chart.html
