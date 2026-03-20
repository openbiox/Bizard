# Skill: Survival Analysis (R)

## Category
Hiplot

## When to use
::: callout-note
**Hiplot website**

## Required R packages
- data.table
- ggplotify
- jsonlite
- survival
- survminer

## Minimal reproducible code
```r
# Survival Analysis
p <- ggsurvplot(
  fit, data = data, risk.table = T, pval = T, conf.int = T, fun = "pct", 
  size = 0.5, xlab = "Time", ylab = "Survival probability",
  ggtheme = theme_bw(), risk.table.y.text.col = TRUE,
  risk.table.height = 0.25, risk.table.y.text = T,
  ncensor.plot = T, ncensor.plot.height = 0.25,
  conf.int.style = "ribbon", surv.median.line = "hv",
  palette = c("#00468BFF", "#ED0000FF"),
  xlim = c(0, 1100), ylim = c(0, 100),
  break.x.by = 150)

p
```

## Full tutorial
https://openbiox.github.io/Bizard/Hiplot/169-survival.html
