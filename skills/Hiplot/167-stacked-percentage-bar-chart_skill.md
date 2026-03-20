# Skill: Percentsge Stacked Bar Chart (R)

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
- scales
- tidyr

## Minimal reproducible code
```r
# Percentsge Stacked Bar Chart
p <- ggplot(data_long, aes(x = percent, y = days, fill = kinds)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_text(aes(label = ifelse(percent != 0, paste0(round(percent), "%"), "")),
            position = position_stack(vjust = 0.5)) +
  labs(title = "Percentage Stacked Bar Chart", x = "Percentage", y = "Days") +
  scale_x_continuous(labels = percent_format(scale = 1)) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_fill_manual(values = c("#E64B35FF","#4DBBD5FF","#00A087FF"))

p
```

## Full tutorial
https://openbiox.github.io/Bizard/Hiplot/167-stacked-percentage-bar-chart.html
