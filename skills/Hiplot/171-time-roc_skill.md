# Skill: Time ROC (R)

## Category
Hiplot

## When to Use
Receiver Operating Characteristic (ROC) analysis with time records in survival analysis.

## Required R Packages
- data.table
- ggplot2
- grid
- jsonlite
- plotROC
- survivalROC

## Minimal Reproducible Code
```r
# Load packages
library(data.table)
library(ggplot2)
library(grid)
library(jsonlite)
library(plotROC)
library(survivalROC)

# Prepare data
# Load data
data1 <- data.table::fread(jsonlite::read_json("https://hiplot.cn/ui/basic/time-roc/data.json")$exampleData$textarea[[1]])
data1 <- as.data.frame(data1)
data2 <- data.table::fread(jsonlite::read_json("https://hiplot.cn/ui/basic/time-roc/data.json")$exampleData$textarea[[2]])
data2 <- as.data.frame(data2)

# convert data structure
surv_table <- data1
colnames(surv_table) <- c("surv", "cens", "risk")
mtime <- as.data.frame(data2)[, 1]
sroc <- lapply(mtime, function(t) {
  stroc <- survivalROC(
    Stime = surv_table$surv,
    status = surv_table$cens,
    marker = surv_table$risk,
    predict.time = t,
    method = "KM"
  )
  data.frame(
    TPF = stroc[["TP"]],
    FPF = stroc[["FP"]],
    cut = stroc[["cut.values"]],
    time = rep(
      stroc[["predict.time"]],
      length(stroc[["TP"]])
    ),
    AUC = rep(
      stroc$AUC,
      length(stroc$FP)
    )
  )
})
mroc <- do.call(rbind, sroc)
mroc$time <- factor(mroc$time)

# View data
head(data1)
head(data2)

# Create visualization
# Time ROC
# ... (see full tutorial for more)
```

## Key Parameters
- `x`: Maps `FPF` to the x aesthetic
- `y`: Maps `TPF` to the y aesthetic
- `color`: Maps `time` to the color aesthetic
- `position`: Position adjustment (identity, dodge, stack, fill)
- `stat`: Statistical transformation to use
- `theme`: Plot theme; tutorial uses `theme_bw()`

## Tips
- Use `theme_minimal()` or `theme_bw()` for clean, publication-ready plots
- Customize color scales with `scale_fill_manual()` or `scale_color_brewer()`
- See the full tutorial for additional customization options and advanced examples

## Full Tutorial
https://openbiox.github.io/Bizard/Hiplot/171-time-roc.html
