# Skill: Interval Area Chart (R)

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
# Interval Area Chart
p <- ggplot(data, aes(x = month, group = 1)) +
  geom_line(aes(y = max_temperature), size = 1.2, color = "#EA3323", 
            linetype = "solid") +
  geom_line(aes(y = min_temperature), size = 1.2, color = "#0000F5", 
            linetype = "solid") +
  geom_line(aes(x = month, y = mean), size = 1.2, color = "#BEBEBE", 
            linetype = "dashed") +
  geom_ribbon(aes(ymin = min_temperature, ymax = max_temperature), 
              fill = "#F2F2F2", alpha = 0.5) +
  geom_text(aes(x = month, y = max_temperature + 1, label = max_temperature),
            color = "#EA3323", size = 2.5, vjust = -0.5, hjust = 0) +
  geom_text(aes(x = month, y = min_temperature - 1, label = min_temperature),
            color = "#0000F5", size = 2.5, vjust = 1.5, hjust = 0) +
  geom_text(aes(x = month, y = mean, label = mean),
            color = "#BEBEBE", size = 2.5, vjust = 1.5, hjust = 0) +
  labs(title = "Temperature", x = "Month", y = "Temperature") +
  scale_color_manual(values = c(max = "#EA3323", min = "#0000F5")) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

p
```

## Full tutorial
https://openbiox.github.io/Bizard/Hiplot/089-interval-area-chart.html
