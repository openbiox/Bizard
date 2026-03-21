# Skill: Pie Group (R)

## Category
Hiplot

## When to Use
Create a Pie Group using R with the Hiplot platform's approach. Suitable for biomedical data visualization with publication-quality output.

## Required R Packages
- cowplot
- data.table
- ggplotify
- jsonlite
- patchwork

## Minimal Reproducible Code
```r
# Load packages
library(cowplot)
library(data.table)
library(ggplotify)
library(jsonlite)
library(patchwork)

# Prepare data
# Load data
data <- data.table::fread(jsonlite::read_json("https://hiplot.cn/ui/basic/pie-group/data.json")$exampleData$textarea[[1]])
data <- as.data.frame(data)

# Convert data structure
data[,"genre"] <- factor(data[,"genre"], levels = unique(data[,"genre"]))
data[,"mpaa"] <- factor(data[,"mpaa"], levels = unique(data[,"mpaa"]))

# View data
head(data)

# Create visualization
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

## Key Parameters
- `fill`: Maps a variable to fill color for group comparison
- `color`: Maps a variable to outline/point color

## Tips
- Adjust text size with `theme(text = element_text(size = 14))` for presentations
- See the full tutorial for additional customization options and advanced examples

## Full Tutorial
https://openbiox.github.io/Bizard/Hiplot/139-pie-group.html
