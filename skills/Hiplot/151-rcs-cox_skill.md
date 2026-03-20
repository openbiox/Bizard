# Skill: RCS-COX (R)

## Category
Hiplot

## When to use
::: callout-note
**Hiplot website**

## Required R packages
- data.table
- ggplot2
- jsonlite
- rms
- stringr
- survival

## Minimal reproducible code
```r
# RCS-COX
p <- ggplot() +
  geom_line(data = orr, aes(main, yhat), linetype = "solid", size = 1, alpha = 1,
            colour = "#FF0000") +
  geom_ribbon(data = orr, aes(main, ymin = lower, ymax = upper), alpha = 0.6, 
              fill = "#FFC0CB") +
  geom_hline(yintercept = 1, linetype = 2, size = 0.5) +
  geom_vline(xintercept = dd$limits$main[2], linetype = 2, size = 0.5) +
  labs(x = " ", y = "Hazard Ratio(95%CI)") +
  theme_bw() +
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
https://openbiox.github.io/Bizard/Hiplot/151-rcs-cox.html
