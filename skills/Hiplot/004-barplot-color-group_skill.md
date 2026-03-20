# Skill: Barplot Color Group (R)

## Category
Hiplot

## When to use
::: callout-note
**Hiplot website**

## Required R packages
- data.table
- ggplot2
- jsonlite
- stringr

## Minimal reproducible code
```r
# Barplot Color Group
p <- ggplot(data = data, aes(x = term, y = count, fill = type)) +
  geom_bar(stat = "identity", width = 0.8) + 
  theme_bw() +
  xlab("Count") +
  ylab("Term") +
  guides(fill = guide_legend(title="Type")) +
  ggtitle("Barplot Color Group") + 
  coord_flip() +
  theme_classic() +
  scale_fill_manual(values = c("#E64B35FF","#4DBBD5FF","#00A087FF","#3C5488FF")) +
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
https://openbiox.github.io/Bizard/Hiplot/004-barplot-color-group.html
