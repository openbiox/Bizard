# Skill: Part highlights the pie chart (R)

## Category
Composition

## When to use
Visualize part highlights the pie chart data in a biomedical context.

## Required R packages
- dplyr
- ggforce
- ggplot2
- ggpubr
- patchwork
- plotrix

## Minimal reproducible code
```r
# Basic pie chart
mycols <- c("#0073C2FF", "#EFC000FF", "#868686FF", "#CD534CFF")
p <- 
  ggplot(count.data, aes(x = "", y = prop, fill = class)) +
  geom_bar(width = 1, stat = "identity", color = "white") +
  coord_polar("y", start = 0)+
  geom_text(aes(y = lab.ypos, label = prop), color = "white")+
  scale_fill_manual(values = mycols) +
  theme_void()

p
```

## Full tutorial
https://openbiox.github.io/Bizard/Composition/PartPieChart.html
