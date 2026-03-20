# Skill: Break Plot (R)

## Category
Distribution

## When to use
Visualize break plot data in a biomedical context.

## Required R packages
- RColorBrewer
- dplyr
- ggbreak
- ggplot2
- ggpubr
- rstatix

## Minimal reproducible code
```r
# Basic BarPlot
p1 <- ggplot(df, aes(x=dose, y=mean_len, fill=supp)) +
  geom_col(position=position_dodge(0.4), width=0.2) +
  geom_errorbar(aes(ymin=mean_len-sd_len, ymax=mean_len+sd_len),
                width=0.1, position=position_dodge(0.4)) +
  scale_y_continuous(breaks = seq(0, 30, 5)) +
  scale_y_cut(breaks=c(15), which=1, scales=1.5) +
  labs(x="Dose (mg/day)", y="Tooth Length (mm)") +
  theme_classic()

p1
```

## Full tutorial
https://openbiox.github.io/Bizard/Distribution/BreakPlot.html
