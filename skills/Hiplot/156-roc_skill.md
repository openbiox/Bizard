# Skill: ROC (R)

## Category
Hiplot

## When to use
::: callout-note
**Hiplot website**

## Required R packages
- data.table
- ggplotify
- jsonlite
- pROC

## Minimal reproducible code
```r
# ROC
col <- c("#00468BFF","#ED0000FF","#42B540FF")
p <- as.ggplot(function() {
  for (i in 1:num_value) {
    if (i == 1) {
      roc_data <- roc(data[, 1], data[, i + 1],
        percent = T, plot = T, grid = T, lty = i, quiet = T,
        print.auc = F, col = col[i], smooth = F,
        main = "ROC Plot"
      )
      text(30, 50, "AUC", font = 2, col = "darkgray")
      text(30, 50 - 10 * i,
        paste(name_val[i], ":", sprintf("%0.4f", as.numeric(roc_data$auc))),
        col = col[i]
      )
    } else {
      roc_data <- roc(data[, 1], data[, i + 1],
        percent = T, plot = T, grid = T, add = T, lty = i, quiet = T,
        print.auc = F, col = col[i]
      )
      text(30, 50 - 10 * i,
        paste(name_val[i], ":", sprintf("%0.4f", as.numeric(roc_data$auc))),
        col = col[i]
      )
    }
  }
    })

p
```

## Full tutorial
https://openbiox.github.io/Bizard/Hiplot/156-roc.html
