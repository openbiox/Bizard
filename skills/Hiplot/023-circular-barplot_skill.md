# Skill: Circular Barplot (R)

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
# Circular Barplot
p <- ggplot(data, aes(x=as.factor(id), y=value, fill=group)) +
  geom_bar(aes(x=as.factor(id), y=value, fill=group), stat="identity", alpha=0.5) +
  ylim(-50,max(na.omit(data$value))+30) +
  geom_segment(data=base_data, aes(x = start, y = -5, xend = end, yend = -5), colour = "black", alpha=0.8, size=0.8 , inherit.aes = FALSE )  +
  geom_text(data=base_data, aes(x = title, y = -12, label=group), colour = "black", alpha=0.8, size=4, fontface="bold", inherit.aes = FALSE) +
  geom_text(data=label_data, aes(x=id, y=value+8, label=individual, hjust=hjust), color="black", fontface="bold",alpha=0.6, size=3, angle= label_data$angle, inherit.aes = FALSE ) +
  geom_text(data=label_data, aes(x=id, y=value-10, label=value, hjust=hjust), color="black", fontface="bold",alpha=0.6, size=3, angle= label_data$angle, inherit.aes = FALSE ) +
  coord_polar() + 
  scale_fill_manual(values = c("#3b4992ff","#ee0000ff","#008b45ff","#631879ff")) +
  theme_minimal() +
  theme(text = element_text(family = "Arial"),
        plot.title = element_text(size = 12,hjust = 0.5),
        axis.title = element_blank(),
        axis.text = element_blank(),
        legend.position = "right",
        legend.direction = "vertical",
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 10),
        panel.grid = element_blank(),
        plot.margin = unit(rep(-1,4), "cm"))

p
```

## Full tutorial
https://openbiox.github.io/Bizard/Hiplot/023-circular-barplot.html
