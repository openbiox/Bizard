# Skill: Pie Group (R)

## Category
Hiplot

## When to use
::: callout-note
**Hiplot website**

## Required R packages
- cowplot
- data.table
- ggplotify
- jsonlite
- patchwork

## Minimal reproducible code
```r
# Pie Group
col <- c("#E64B35FF","#4DBBD5FF","#00A087FF","#3C5488FF","#F39B7FFF","#8491B4FF",
         "#91D1C2FF","#DC0000FF","#7E6148FF","#B09C85FF")
plist <- list()
for (i in 1:length(unique(data[,"mpaa"]))) {
  data_tmp <- data[data[,"mpaa"] == unique(data[,"mpaa"])[i],]
  x <- table(data_tmp[,"genre"])
  ptmp <- as.ggplot(function(){
    par(oma=c(0,0,0,0))
    pie(x,
      labels = sprintf("%s\n(n=%s, %s%%)", names(x), x,
        round(x / sum(x) * 100, 0)),
      col = col,
      main = paste0("mpaa", ":", unique(data[,"mpaa"])[i]),
      edges = 200,
      radius = 0.8,
      clockwise = F
    )
  })
  plist[[i]] <- ptmp
}

plot_grid(plotlist = plist, ncol = 2)
```

## Full tutorial
https://openbiox.github.io/Bizard/Hiplot/139-pie-group.html
