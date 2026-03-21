# Skill: RCS-LRM (R)

## Category
Hiplot

## When to Use
Nonlinear regression analysis.

## Required R Packages
- data.table
- ggplot2
- jsonlite
- rms
- stringr
- survival

## Minimal Reproducible Code
```r
# Load packages
library(data.table)
library(ggplot2)
library(jsonlite)
library(rms)
library(stringr)
library(survival)

# Prepare data
# Load data
data <- data.table::fread(jsonlite::read_json("https://hiplot.cn/ui/basic/rcs-lrm/data.json")$exampleData$textarea[[1]])
data <- as.data.frame(data)

# Convert data structure
data <- na.omit(data)
ex <- set::not(colnames(data), c("main", "group"))
ex <- str_c(ex, collapse = "+")
dd <<- datadist(data)
options(datadist = "dd")
for (i in 3:5) {
  fit <- lrm(as.formula(paste0("group~rcs(main,nk=i,inclx = T)+", ex, collapse = "+")), data = data, x = TRUE)
  tmp <- AIC(fit)
  if (i == 3) {
    AIC <- tmp
    nk <<- 3
  }
  if (tmp < AIC) {
    AIC <- tmp
    nk <<- i
  }
}
fit <- lrm(as.formula(paste0("group~rcs(main,nk=nk,inclx = T)+", ex, collapse = "+")), data = data, x = TRUE)
dd$limits$main[2] <- median(data$main)
fit <- update(fit)
orr <- Predict(fit, main, fun = exp, ref.zero = TRUE)

# View data
head(data)

# Create visualization
# RCS-LRM
p <- ggplot() +
  geom_line(data = orr, aes(main, yhat), linetype = "solid", size = 1, alpha = 1,
            colour = "#FF0000") +
  geom_ribbon(data = orr, aes(main, ymin = lower, ymax = upper), alpha = 0.6, 
              fill = "#FFC0CB") +
  geom_hline(yintercept = 1, linetype = 2, size = 0.5) +
  geom_vline(xintercept = dd$limits$main[2], linetype = 2, size = 0.5) +
  labs(x = "main", y = "Odds Ratio(95%CI)") +
  theme_bw() +
# ... (see full tutorial for more)
```

## Key Parameters
- `alpha`: Controls transparency (0 = fully transparent, 1 = opaque)
- `position`: Position adjustment (identity, dodge, stack, fill)
- `theme`: Plot theme; tutorial uses `theme_bw()`

## Tips
- Use `theme_minimal()` or `theme_bw()` for clean, publication-ready plots
- Adjust text size with `theme(text = element_text(size = 14))` for presentations
- See the full tutorial for additional customization options and advanced examples

## Full Tutorial
https://openbiox.github.io/Bizard/Hiplot/152-rcs-lrm.html
