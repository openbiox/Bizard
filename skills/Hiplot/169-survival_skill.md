# Skill: Survival Analysis (R)

## Category
Hiplot

## When to Use
The survivorship curve is a graph showing the number or proportion of individuals surviving to each age for a given species or group (e.g. males or females).

## Required R Packages
- data.table
- ggplotify
- jsonlite
- survival
- survminer

## Minimal Reproducible Code
```r
# Load packages
library(data.table)
library(ggplotify)
library(jsonlite)
library(survival)
library(survminer)

# Prepare data
# Load data
data <- data.table::fread(jsonlite::read_json("https://hiplot.cn/ui/basic/survival/data.json")$exampleData[[1]]$textarea[[1]])
data <- as.data.frame(data)

# convert data structure
colnames(data) <- c("Time", "Status", "Group")
data[,1] <- as.numeric(data[,1])
fit <- survfit(Surv(Time, Status == 1) ~ Group, data = data)
data <- data[data[,1] < 1100,]

# View data
head(data)

# Create visualization
# Survival Analysis
p <- ggsurvplot(
  fit, data = data, risk.table = T, pval = T, conf.int = T, fun = "pct", 
  size = 0.5, xlab = "Time", ylab = "Survival probability",
  ggtheme = theme_bw(), risk.table.y.text.col = TRUE,
  risk.table.height = 0.25, risk.table.y.text = T,
  ncensor.plot = T, ncensor.plot.height = 0.25,
  conf.int.style = "ribbon", surv.median.line = "hv",
  palette = c("#00468BFF", "#ED0000FF"),
  xlim = c(0, 1100), ylim = c(0, 100),
  break.x.by = 150)

p
```

## Key Parameters
- `theme`: Plot theme; tutorial uses `theme_bw()`
- `fill`: Maps a variable to fill color for group comparison
- `color`: Maps a variable to outline/point color

## Tips
- Use `theme_minimal()` or `theme_bw()` for clean, publication-ready plots
- Adjust text size with `theme(text = element_text(size = 14))` for presentations
- See the full tutorial for additional customization options and advanced examples

## Full Tutorial
https://openbiox.github.io/Bizard/Hiplot/169-survival.html
