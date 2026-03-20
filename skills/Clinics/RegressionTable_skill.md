# Skill: Regression Analysis Table (R)

## Category
Clinics

## When to use
The regression analysis table is used to display the results of the regression model. It provides statistical information about the variables in the model and helps explain the relationship between the variables.

## Required R packages
- broom.helpers
- datawizard
- dplyr
- gtsummary
- survival

## Minimal reproducible code
```r
# Basic regression analysis table
t1 <- coxph(Surv(time, status) ~ albumin + sex + age,
  data = df
) %>%
  tbl_regression()

t1
```

## Full tutorial
https://openbiox.github.io/Bizard/Clinics/RegressionTable.html
