# Skill: Nomogram (R)

## Category
Hiplot

## When to Use
Nomogram is often used to evaluate the prognosis of oncology and medicine, and can visualize the results of logistic regression or Cox regression.

## Required R Packages
- data.table
- ggplotify
- jsonlite
- rms
- survival

## Minimal Reproducible Code
```r
# Load packages
library(data.table)
library(ggplotify)
library(jsonlite)
library(rms)
library(survival)

# Prepare data
# Load data
data <- data.table::fread(jsonlite::read_json("https://hiplot.cn/ui/basic/nomogram/data.json")$exampleData$textarea[[1]])
data <- as.data.frame(data)

# Convert data structure
dd <- datadist(data)
options(datadist = "dd")
## Build COX model and run nomogram
cox_res <- psm(
  data = data,
  as.formula(paste(
    sprintf("Surv(%s, %s) ~ ", colnames(data)[1], colnames(data)[2]),
    paste(colnames(data)[3:length(colnames(data))],
      collapse = "+"
    )
  )),
  # Surv(time, status) ~ age + sex + ph.ecog + ph.karno + pat.karno,
  dist = "lognormal"
)
## Build survival probability function
surv <- Survival(cox_res)
## Build quantile survival time function
med <- Quantile(cox_res)

cox_nomo <- nomogram(
  cox_res,
  fun = list(function(x) surv(365, x), function(x) surv(1095, x),
             function(x) surv(1825, x), function(x) med(lp = x)),
  funlabel = c("1-year Survival Probability",
               "3-year Survival Probability",
               "5-year Survival Probability",
               "Median Survival Time"),
  maxscale = 100
)

# View data
head(data)

# Create visualization
# Nomogram
p <- as.ggplot(function() {
  plot(cox_nomo, scale = 1)
# ... (see full tutorial for more)
```

## Key Parameters
- `stat`: Statistical transformation to use
- `fill`: Maps a variable to fill color for group comparison
- `color`: Maps a variable to outline/point color

## Tips
- Adjust text size with `theme(text = element_text(size = 14))` for presentations
- See the full tutorial for additional customization options and advanced examples

## Full Tutorial
https://openbiox.github.io/Bizard/Hiplot/131-nomogram.html
