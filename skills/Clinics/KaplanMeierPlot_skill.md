# Skill: Kaplan Meier Plot (R)

## Category
Clinics

## When to Use
Create a Kaplan Meier Plot visualization in R for biomedical data analysis and research publications.

## Required R Packages
- dplyr
- ggplot2
- patchwork
- survival
- survminer
- tidyr
- zoo

## Minimal Reproducible Code
```r
# Load packages
library(dplyr)
library(ggplot2)
library(patchwork)
library(survival)
library(survminer)
library(tidyr)

# Prepare data
# Using the built-in lung dataset (from the survival package)
data("lung")
# Data preprocessing
surv_data <- lung %>%
  mutate(
    status = ifelse(status == 2, 1, 0),  # Transition state code (1=event)
    sex = factor(sex, labels = c("Male", "Female")),
    group = sample(c("Treatment", "Placebo"), n(), replace = TRUE)
  )

# View data structure
glimpse(surv_data)

# Survival time distribution
summary(surv_data$time)

# Fitting survival curves
fit <- survfit(Surv(time, status) ~ group, data = surv_data)

# Extract curve data
surv_curve <- surv_summary(fit) 


# Calculate the log-rank test P value
diff <- survdiff(Surv(time, status) ~ group, data = surv_data)
p_value <- signif(1 - pchisq(diff$chisq, length(diff$n)-1), 3)

# Create visualization
# Basic survival curve
p1 <- ggplot(surv_curve, aes(x = time, y = surv, color = strata)) +
  geom_step(linewidth = 1) +
  labs(x = "Time (Months)", y = "Survival Probability") +
  scale_y_continuous(labels = scales::percent)
p1
```

## Key Parameters
- `x`: Maps `time` to the x aesthetic
- `y`: Maps `surv` to the y aesthetic
- `color`: Maps `group` to the color aesthetic
- `fill`: Maps `group` to the fill aesthetic
- `alpha`: Controls transparency (0 = fully transparent, 1 = opaque)
- `width`: Controls element width
- `position`: Position adjustment (identity, dodge, stack, fill)
- `stat`: Statistical transformation to use

## Tips
- The tutorial includes a '2. More advanced plot' section with advanced styling options
- Use `theme_minimal()` or `theme_bw()` for clean, publication-ready plots
- Customize color scales with `scale_fill_manual()` or `scale_color_brewer()`
- Follow CONSORT or STROBE guidelines for clinical data visualization where applicable

## Full Tutorial
https://openbiox.github.io/Bizard/Clinics/KaplanMeierPlot.html
