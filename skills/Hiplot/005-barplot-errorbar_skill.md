# Skill: Barplot (errorbar) (R)

## Category
Hiplot

## When to use
::: callout-note
**Hiplot website**

## Required R packages
- Rmisc
- data.table
- ggplot2
- ggpubr
- jsonlite

## Minimal reproducible code
```r
# Barplot (errorbar)
p <- ggplot(data_sd, aes(x = data_sd[, 1], y = data_sd[, 3], fill = data_sd[, 1])) +
  geom_bar(stat = "identity", color = "black", 
           position = position_dodge(), alpha = 1) +
  geom_errorbar(aes(ymin = data_sd[, 3] - sd, ymax = data_sd[, 3] + sd),
                width = 0.2,
                position = position_dodge(0.9)) +
  labs(title = "Barplot (errorbar)", x = colnames(data_sd)[1], 
       y = colnames(data_sd)[3], fill = colnames(data_sd)[1]) +
  geom_jitter(data = data, aes(data[, 2], data[, 1], fill = data[, 2]), size = 2, fill = "black", pch = 19, width = 0.2) +
  stat_compare_means(data = data, aes(data[, 2], data[, 1], fill = data[, 2]),
                     label = "p.format", ref.group = ".all.", vjust = 1, 
                     method = "t.test") +
  scale_fill_manual(values = c("#E64B35FF","#4DBBD5FF","#00A087FF","#3C5488FF")) +
  theme_bw() +
  ylim(0,100) +
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
https://openbiox.github.io/Bizard/Hiplot/005-barplot-errorbar.html
