# Skill: Chi-square-fisher Test (R)

## Category
Hiplot

## When to Use
Chi-square and Fisher test can be used to test the frequency difference of categorical variables. The tool will automatically select the statistical method of Chi-square and Fisher exact test.

## Required R Packages
- aplot
- data.table
- ggplot2
- jsonlite
- visdat

## Minimal Reproducible Code
```r
# Load packages
library(aplot)
library(data.table)
library(ggplot2)
library(jsonlite)
library(visdat)

# Prepare data
# Load data
data <- data.table::fread(jsonlite::read_json("https://hiplot.cn/ui/basic/chi-square-fisher/data.json")$exampleData[[1]]$textarea[[1]])
data <- as.data.frame(data)

# convert data structure
rownames(data) <- data[,1]
data <- data[,-1]
cb <- combn(nrow(data), 2)
final <- data.frame()
for (i in 1:ncol(cb)) {
  tmp <- data[cb[,i],]
  groups <- paste0(rownames(data)[cb[,i]], collapse = " | ")
  
  res <- tryCatch({
    chisq.test(tmp)
    }, warning = function(w) {
      tryCatch({fisher.test(tmp)}, error = function(e) {
        return(fisher.test(tmp, simulate.p.value = TRUE))
        })
  })
  val_percent <- apply(tmp, 1, function(x) {
    sprintf("%s (%s%%)", x, round(x / sum(x), 2) * 100)
  })
  val_percent1 <- paste0(colnames(tmp), ":", val_percent[,1])
  val_percent1 <- paste0(val_percent1, collapse = " | ")
  val_percent2 <- paste0(colnames(tmp), ":", val_percent[,2])
  val_percent2 <- paste0(val_percent2, collapse = " | ")
  tmp <- data.frame(
    groups = groups,
    val_percent_left = val_percent1,
    val_percent_right = val_percent2,
    statistic = ifelse(is.null(res$statistic), NA,
      as.numeric(res$statistic)),
    pvalue = as.numeric(res$p.value),
    method = res$method
  )
  final <- rbind(final, tmp)
}
final <- as.data.frame(final)
final$pvalue < as.numeric(final$pvalue)
final$statistic < as.numeric(final$statistic)

# ... (see full tutorial for more)
```

## Key Parameters
- `stat`: Statistical transformation to use
- `fill`: Maps a variable to fill color for group comparison
- `color`: Maps a variable to outline/point color

## Tips
- Customize color scales with `scale_fill_manual()` or `scale_color_brewer()`
- Adjust text size with `theme(text = element_text(size = 14))` for presentations
- See the full tutorial for additional customization options and advanced examples

## Full Tutorial
https://openbiox.github.io/Bizard/Hiplot/019-chi-square-fisher.html
