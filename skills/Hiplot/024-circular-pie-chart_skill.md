# Skill: Circular Pie Chart (R)

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
# Circular Pie Chart
p <- ggplot(data, aes(x = draw_class, y = values, fill = labels)) +
  geom_bar(position = "stack", stat = "identity", width = 0.7) +
  geom_text(data = filtered_data, aes(label = sprintf("%.2f%%", draw_percent)),
            position = position_stack(vjust = 0.5), size = 3) +
  coord_polar(theta = "y") +
  xlab("") +
  ylab("Pie Chart") +
  scale_fill_manual(values = c("#e64b35ff","#4dbbd5ff","#00a087ff","#3c5488ff","#f39b7fff")) +
  theme_minimal() +
  theme(text = element_text(family = "Arial"),
        plot.title = element_text(size = 12,hjust = 0.5),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 10),
        axis.text.x = element_text(color = "black"),
        axis.text.y = element_blank(),
        legend.position = "right",
        legend.direction = "vertical",
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 10),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

p
```

## Full tutorial
https://openbiox.github.io/Bizard/Hiplot/024-circular-pie-chart.html
