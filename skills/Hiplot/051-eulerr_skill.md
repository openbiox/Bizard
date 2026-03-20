# Skill: Eulerr Plot (R)

## Category
Hiplot

## When to use
::: callout-note
**Hiplot website**

## Required R packages
- data.table
- eulerr
- ggplotify
- jsonlite

## Minimal reproducible code
```r
# Eulerr Plot
fill <- c("#3B4992FF","#EE0000FF","#008B45FF","#631879FF","#008280FF","#BB0021FF",
          "#5F559BFF","#A20056FF")
p <- as.ggplot(
  plot(euler_set,
    labels = list(col = rep("white", length(genes))),
    fills = list(fill = fill),
    quantities = list(type = c("percent", "counts"),
    col = rep("white", length(genes))),
    main = "Eulerr")
)

p
```

## Full tutorial
https://openbiox.github.io/Bizard/Hiplot/051-eulerr.html
