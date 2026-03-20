# Skill: Corrplot (R)

## Category
Hiplot

## When to use
::: callout-note
**Hiplot website**

## Required R packages
- corrplot
- data.table
- ggcorrplot
- ggplotify
- jsonlite

## Minimal reproducible code
```r
# Corrplot
p <- as.ggplot(function(){
  corrplot(
    corr, 
    method = "circle", 
    type = "upper",
    tl.col = "black", 
    diag = F,
    col = colorRampPalette(c("#4477AA", "#FFFFFF", "#BB4444"))(200),
    order = "hclust",
    hclust.method = "ward.D2")
  }) +
  xlab("") + ylab("") +
  ggtitle("Cor Heatmap Plot") +
  theme_void() +
  theme(text = element_text(family = "Arial"),
        plot.title = element_text(size = 12,hjust = 0.5),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 10),
        axis.text.x = element_blank(), axis.text.y = element_blank(),
        legend.position = "right",
        legend.direction = "vertical",
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 10))
p
```

## Full tutorial
https://openbiox.github.io/Bizard/Hiplot/033-corrplot.html
