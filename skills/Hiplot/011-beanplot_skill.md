# Skill: Beanplot (R)

## Category
Hiplot

## When to use
::: callout-note
**Hiplot website**

## Required R packages
- beanplot
- data.table
- ggplotify
- jsonlite

## Minimal reproducible code
```r
# Beanplot
p <- as.ggplot(function() {
  beanplot(Y ~ reorder(X, GroupOrder, mean), data = data, ll = 0.04,
           main = "Bean Plot", ylab = "Y", xlab = "X", side = "both",
           border = NA, horizontal = F, 
           col = list(c("#2b70c4", "#2b70c4"),c("#e9c216", "#e9c216")),
           beanlines = "mean", overallline = "mean", kernel = "gaussian")
  
  legend("bottomright", fill = c("#2b70c4", "#e9c216"),
         legend = levels(factor(data[, 3])))
})

p
```

## Full tutorial
https://openbiox.github.io/Bizard/Hiplot/011-beanplot.html
