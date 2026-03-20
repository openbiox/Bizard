# Skill: Time ROC (R)

## Category
Hiplot

## When to use
::: callout-note
**Hiplot website**

## Required R packages
- data.table
- ggplot2
- grid
- jsonlite
- plotROC
- survivalROC

## Minimal reproducible code
```r
# Time ROC
col <- c("#E64B35FF","#4DBBD5FF","#00A087FF","#3C5488FF","#F39B7FFF")
p <- ggplot(mroc, aes(x = FPF, y = TPF, label = cut, color = time)) +
  plotROC::geom_roc(labels = FALSE, stat = "identity", n.cuts = 0) +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = 2) +
  labs(title = "ROC Dependence Time", x = "False positive rate",
       y = "True positive rate", 
       color = paste("Time", "(", "Year", ")")) +
  theme_bw() +
  theme(text = element_text(family = "Arial"),
        plot.title = element_text(size = 12, hjust = 0.5),
        axis.title = element_text(size = 10),
        legend.position = "right",
        legend.direction = "vertical",
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 10)) +
  scale_color_manual(values = col)

auc <- levels(factor(mroc$AUC))
for (i in 1:length(auc)) {
  p <- p + annotate("text",
    x = 0.75,
    y = 0.05 + 0.05 * i, ## 注释text的位置
    col = col[i],
    label = paste(
      paste(paste(mtime[i], "Year", sep = " "), " = "),
      round(as.numeric(auc[i]), 2)
    )
  )
}

p
```

## Full tutorial
https://openbiox.github.io/Bizard/Hiplot/171-time-roc.html
