# Skill: Radial Column Chart (R)

## Category
Distribution

## When to Use
Create a Radial Column Chart visualization in R for biomedical data analysis and research publications.

## Required R Packages
- dplyr
- ggforce
- ggplot2
- scales

## Minimal Reproducible Code
```r
# Load packages
library(dplyr)
library(ggforce)
library(ggplot2)
library(scales)

# Prepare data
# Data reading and processing code can be displayed freely------
# Generate simulated clinical data
set.seed(123)
n <- 12  # Sample size

df <- data.frame(
  id = 1:n,
  patient = paste0("P-", sprintf("%02d", 1:n)),
  value = c(rnorm(6, 80, 15), rnorm(6, 120, 20)),  # Control group and treatment group
  group = rep(c("Control", "Treatment"), each = 6)
) %>%
  mutate(
    angle = 90 - 360 * (id - 0.5)/n,
    hjust = ifelse(angle < -90, 1, 0),
    angle = ifelse(angle < -90, angle + 180, angle)
  )

# Adding built-in datasets
data("iris")

# Create visualization
# Basic histogram
p1 <- ggplot(df, aes(x = factor(id), y = value)) +
  geom_col(aes(fill = group), width = 0.8, alpha = 0.8) +
  coord_radial(inner.radius = 0.3) +
  scale_fill_manual(values = c("#1E88E5", "#D81B60")) +
  theme_void() +
  labs(title = "Comparison of indicators between the treatment group and the control group")
p1
```

## Key Parameters
- `x`: Maps `id` to the x aesthetic
- `fill`: Maps `group` to the fill aesthetic
- `y`: Maps `value` to the y aesthetic
- `alpha`: Controls transparency (0 = fully transparent, 1 = opaque)
- `width`: Controls element width
- `position`: Position adjustment (identity, dodge, stack, fill)
- `theme`: Plot theme; tutorial uses `theme_void()`

## Tips
- The tutorial includes a '2. More advanced charts' section with advanced styling options
- Customize color scales with `scale_fill_manual()` or `scale_color_brewer()`
- Consider adding `geom_jitter()` or raw data points alongside distribution plots for small sample sizes

## Full Tutorial
https://openbiox.github.io/Bizard/Distribution/RadialColumnChart.html
