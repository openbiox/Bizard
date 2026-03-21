# Skill: Radar/Spider Plot (R)

## Category
Ranking

## When to Use
A radar chart, spider chart, or web chart is a two-dimensional chart type used to plot a series of values over one or more quantitative variables. The fmsb library is an excellent tool for building this type of chart in R.

## Required R Packages
- fmsb

## Minimal Reproducible Code
```r
# Load packages
library(fmsb)

# Prepare data
# 1.R built-in data - iris
head(iris)

# 2.Self-built dataset
## Here we create a data set about the performance of three students in different subjects
set.seed(99)                  
data <- as.data.frame(matrix( sample( 0:20 , 15 , replace=F) , ncol=5))
colnames(data) <- c("math" , "english" , "biology" , "music" , "R-coding" )
rownames(data) <- paste("mister" , letters[1:3] , sep="-")
data <- rbind(rep(20,5) , rep(0,5) , data) 

# 3.TCGA database (gene expression data of liver cancer)
tcga_group_radar <- readr::read_csv(
"https://bizard-1301043367.cos.ap-guangzhou.myqcloud.com/tcga_group_radar.csv")
tcga_simple_bar <- data.frame(tcga_group_radar[3,])

# Create visualization
# Data collation
iris_setosa <- iris[c(1:50),]
iris_setosa <- iris_setosa[,-5]
iris_setosa_radar <- rbind(rep(6,4),rep(0,4),iris_setosa)
# plot
par(mar = c(1, 1, 1, 1))
radarchart(iris_setosa_radar)
```

## Key Parameters
- `fill`: Maps a variable to fill color for group comparison
- `color`: Maps a variable to outline/point color

## Tips
- Adjust text size with `theme(text = element_text(size = 14))` for presentations
- Sort categories by value rather than alphabetically for clearer ranking visualization
- See the full tutorial for additional customization options and advanced examples

## Full Tutorial
https://openbiox.github.io/Bizard/Ranking/Radar.html
