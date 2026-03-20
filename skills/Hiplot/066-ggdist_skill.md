# Skill: Dist Plot (R)

## Category
Hiplot

## When to use
::: callout-note
**Hiplot website**

## Required R packages
- broom
- data.table
- ggdist
- ggplot2
- jsonlite
- modelr
- tidyr

## Minimal reproducible code
```r
# Dist Plot
p <- ggplot(data3, aes_(y = as.name(colnames(data[1])))) +
  stat_dist_halfeye(aes(dist = "student_t", arg1 = df.residual(data2),
                        arg2 = .fitted, arg3 = .se.fit),
                    scale = .5) +
  geom_point(aes_(x = as.name(colnames(data[2]))),
             data = data, pch = "|", size = 2,
             position = position_nudge(y = -.15)) +
  ggtitle("ggdist Plot") + 
  xlab("response") + ylab("condition") +
  theme_ggdist() +
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
https://openbiox.github.io/Bizard/Hiplot/066-ggdist.html
