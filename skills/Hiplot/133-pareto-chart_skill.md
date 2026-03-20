# Skill: Pareto Chart (R)

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
# Pareto Chart
p <- ggplot(data, aes(x = channel, y = sales, fill = channel)) +
  geom_bar(stat = "identity") +
  geom_line(aes(y = accumulating), group = 1) +
  geom_point(aes(y = accumulating), show.legend = FALSE) +
  scale_y_continuous(sec.axis = sec_axis(trans = ~ . / max_y * 100, name = "Percentage")) +
  scale_fill_manual(values = c("#E64B35FF","#4DBBD5FF","#00A087FF","#3C5488FF",
                               "#F39B7FFF","#8491B4FF","#91D1C2FF","#DC0000FF")) +
  theme_bw()

p
```

## Full tutorial
https://openbiox.github.io/Bizard/Hiplot/133-pareto-chart.html
