# Skill: Kaplan Meier Plot (R)

## Category
Clinics

## When to use
Visualize kaplan meier plot data in a biomedical context.

## Required R packages
- dplyr
- ggplot2
- patchwork
- survival
- survminer
- tidyr
- zoo

## Minimal reproducible code
```r
# Basic survival curve
p1 <- ggplot(surv_curve, aes(x = time, y = surv, color = strata)) +
  geom_step(linewidth = 1) +
  labs(x = "Time (Months)", y = "Survival Probability") +
  scale_y_continuous(labels = scales::percent)
p1
```

## Full tutorial
https://openbiox.github.io/Bizard/Clinics/KaplanMeierPlot.html
