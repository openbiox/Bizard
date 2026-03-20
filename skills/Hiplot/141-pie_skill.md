# Skill: Pie (R)

## Category
Hiplot

## When to use
::: callout-note
**Hiplot website**

## Required R packages
- data.table
- dplyr
- ggplot2
- jsonlite

## Minimal reproducible code
```r
# Pie
p <- ggplot(data, aes(x = "", y = Value, fill = Group)) +
  geom_col(width = 1) +
  geom_bar(stat = "identity", width = 1, color = "white") +
  geom_text(aes(y = ypos, 
                label = sprintf("%s\n(n=%s, %s%%)", Group, Value,
                                round(Value / sum(data$Value) * 100, 2))), 
            color = "white", fontface = "bold") +
  coord_polar(theta = "y", start = 0, direction = -1) +
  guides(fill = guide_legend(title = "Group")) +
  scale_fill_discrete(
    breaks = data$Group,
    labels = paste(data$Group," (", round(data$Value / sum(data$Value) * 100, 2),
                   "%)", sep = "")) +
  scale_fill_manual(values = c("#00468BFF","#ED0000FF","#42B540FF","#0099B4FF")) +
  ggtitle("Pie Plot") + 
  theme_minimal() +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    panel.border = element_blank(),
    panel.grid = element_blank(),
    axis.ticks = element_blank(),
    plot.title = element_text(size = 14, face = "bold",
                              hjust = 0.5, vjust = -1),
    legend.position = "none"
  )
    
  
p
```

## Full tutorial
https://openbiox.github.io/Bizard/Hiplot/141-pie.html
