# Skill: Regression Analysis Table (R)

## Category
Clinics

## When to Use
The regression analysis table is used to display the results of the regression model. It provides statistical information about the variables in the model and helps explain the relationship between the variables.

## Required R Packages
- broom.helpers
- datawizard
- dplyr
- gtsummary
- survival

## Minimal Reproducible Code
```r
# Load packages
library(broom.helpers)
library(datawizard)
library(dplyr)
library(gtsummary)
library(survival)

# Prepare data
df <- pbc %>%
  filter(status != 1) %>%
  mutate(status = ifelse(status == 2, 1, 0)) %>%
  select(2:13) %>%
  na.omit() %>% 
  # Divide `albumin` into 3 groups
  mutate(albumin3cat = categorize(albumin, split = "quantile", n_groups = 3))

head(df[,1:6])

# Create visualization
# Basic regression analysis table
t1 <- coxph(Surv(time, status) ~ albumin + sex + age,
  data = df
) %>%
  tbl_regression()

t1
```

## Key Parameters
- `stat`: Statistical transformation to use
- `fill`: Maps a variable to fill color for group comparison
- `color`: Maps a variable to outline/point color

## Tips
- Adjust text size with `theme(text = element_text(size = 14))` for presentations
- Follow CONSORT or STROBE guidelines for clinical data visualization where applicable
- See the full tutorial for additional customization options and advanced examples

## Full Tutorial
https://openbiox.github.io/Bizard/Clinics/RegressionTable.html
