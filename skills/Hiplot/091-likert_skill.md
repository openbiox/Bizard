# Skill: Likert Plot (R)

## Category
Hiplot

## When to Use
::: callout-note
**Hiplot website**

## Required R Packages
- data.table
- ggplotify
- jsonlite
- likert

## Minimal Reproducible Code
```r
# Likert Plot
pobj <- likert(data)
colrs <- c("#3B4992FF","#EE0000FF")
p <- as.ggplot(plot(pobj, type = "bar", 
                    low.color = colrs[1], high.color = colrs[2], wrap = 50))

p
```

## Full Tutorial
https://openbiox.github.io/Bizard/Hiplot/091-likert.html
