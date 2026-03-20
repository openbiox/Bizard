# Skill: Dumbbell Chart (R)

## Category
Hiplot

## When to use
::: callout-note
**Hiplot website**

## Required R packages
- data.table
- ggalt
- ggplot2
- jsonlite

## Minimal reproducible code
```r
# Dumbbell Chart
colors <- c("#3B4992FF","#EE0000FF")
p <- ggplot(data, aes(y = reorder(country, y1952), x = y1952, xend = y2007)) +
  geom_dumbbell(size = 1, size_x = 3,  size_xend = 3, colour = "#AFAFAF",  
                colour_x = colors[1],  colour_xend = colors[2]) +
  labs(title = "Dummbbell Chart", x = "Life Expectancy (years)",
       y = "country") +
  theme_minimal() +
  theme(text = element_text(family = "Arial"),
        plot.title = element_text(size = 12,hjust = 0.5),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 10),
        axis.text.x = element_text(angle = 0, hjust = 0.5,vjust = 1),
        legend.position = "right",
        legend.direction = "vertical",
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 10))

p
```

## Full tutorial
https://openbiox.github.io/Bizard/Hiplot/048-dumbbell.html
