# Skill: Break Plot (R)

## Category
Distribution

## When to Use
Create a Break Plot visualization in R for biomedical data analysis and research publications.

## Required R Packages
- RColorBrewer
- dplyr
- ggbreak
- ggplot2
- ggpubr
- rstatix

## Minimal Reproducible Code
```r
# Load packages
library(RColorBrewer)
library(dplyr)
library(ggbreak)
library(ggplot2)
library(ggpubr)
library(rstatix)

# Prepare data
# Data Preparation
df <- ToothGrowth %>%
  group_by(supp, dose) %>%
  summarise(
    mean_len = mean(len),
    sd_len = sd(len),
    n = n(),
    se_len = sd_len/sqrt(n),
    .groups = 'drop')


# Statistical tests (key repair points)
stat.test <- ToothGrowth %>%
  group_by(dose) %>%
  t_test(len ~ supp) %>%
  add_xy_position(x = "dose", dodge = 0.8)

head(df)

# Create visualization
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

## Key Parameters
- `x`: Maps `dose` to the x aesthetic
- `y`: Maps `len` to the y aesthetic
- `fill`: Maps `supp` to the fill aesthetic
- `color`: Maps `supp` to the color aesthetic
- `alpha`: Controls transparency (0 = fully transparent, 1 = opaque)
- `width`: Controls element width
- `position`: Position adjustment (identity, dodge, stack, fill)
- `stat`: Statistical transformation to use

## Tips
- The tutorial includes a '4. More advanced charts' section with advanced styling options
- Use `theme_minimal()` or `theme_bw()` for clean, publication-ready plots
- Customize color scales with `scale_fill_manual()` or `scale_color_brewer()`
- Consider adding `geom_jitter()` or raw data points alongside distribution plots for small sample sizes

## Full Tutorial
https://openbiox.github.io/Bizard/Distribution/BreakPlot.html
