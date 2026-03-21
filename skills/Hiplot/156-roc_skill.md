# Skill: ROC (R)

## Category
Hiplot

## When to Use
Receiver operating characteristic curve (ROC curve) is used to describe the diagnostic ability of binary classifier system when its recognition threshold changes.

## Required R Packages
- data.table
- ggplotify
- jsonlite
- pROC

## Minimal Reproducible Code
```r
# Load packages
library(data.table)
library(ggplotify)
library(jsonlite)
library(pROC)

# Prepare data
# Load data
data <- data.table::fread(jsonlite::read_json("https://hiplot.cn/ui/basic/roc/data.json")$exampleData$textarea[[1]])
data <- as.data.frame(data)

# Convert data structure
name_val <- colnames(data)[2:ncol(data)]
num_value <- ncol(data) - 1

# View data
head(data)

# Create visualization
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

## Key Parameters
- `fill`: Maps a variable to fill color for group comparison
- `color`: Maps a variable to outline/point color

## Tips
- Adjust text size with `theme(text = element_text(size = 14))` for presentations
- See the full tutorial for additional customization options and advanced examples

## Full Tutorial
https://openbiox.github.io/Bizard/Hiplot/156-roc.html
