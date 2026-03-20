# Skill: Fishplot (R)

## Category
Hiplot

## When to use
::: callout-note
**Hiplot website**

## Required R packages
- data.table
- fishplot
- jsonlite

## Minimal reproducible code
```r
## Create a fish object
fish = createFishObject(as.matrix(data[,4:7]), parents=data$parents, 
                        timepoints=data$timepoints, 
                        col = c("#888888","#e8130c","#f8150d","#55158f"))
## Calculate the layout of the drawing
fish = layoutClones(fish)
## Draw the plot, using the splining method (recommended), and providing both timepoints to label and a plot title
fishPlot(fish,shape="spline", title.btm="Sample1", title = "Fishplot",
         cex.title=1, vlines=c(0,30,75,150), 
         vlab=c("Day 0","Day 30","Day 75","Day 150"))
```

## Full tutorial
https://openbiox.github.io/Bizard/Hiplot/055-fishplot.html
