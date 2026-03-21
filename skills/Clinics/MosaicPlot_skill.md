# Skill: Mosaic Plot (R)

## Category
Clinics

## When to Use
Create a Mosaic Plot visualization in R for biomedical data analysis and research publications.

## Required R Packages
- dplyr
- ggplot2
- plyr
- reshape2
- tidyr
- vcd
- wesanderson

## Minimal Reproducible Code
```r
# Load packages
library(dplyr)
library(ggplot2)
library(plyr)
library(reshape2)
library(tidyr)
library(vcd)

# Prepare data
# Generate simulated data
df <- data.frame(segment = c("Patient1", "Patient2", "Patient3","Patient4"),
                 "Macrophage" = c(2400	,1200,	600	,250),
                 "Epithelial" = c(1000	,900,	600,	250),
                 "T cells" = c(400,	600	,400,	250),
                 "B cells" = c(200,	300	,400,	250))

melt_df<-melt(df,id="segment")
# Convert numbers to percentages
segpct<-rowSums(df[,2:ncol(df)])
for (i in 1:nrow(df)){
  for (j in 2:ncol(df)){
    df[i,j]<-df[i,j]/segpct[i]*100  
  }
}

segpct<-segpct/sum(segpct)*100
df$xmax <- cumsum(segpct)
df$xmin <- (df$xmax - segpct)

dfm <- melt(df, id = c("segment", "xmin", "xmax"),value.name="percentage")
colnames(dfm)[ncol(dfm)]<-"percentage"

# The ddply() function uses a custom statistical function to group and calculate data.frame
dfm1 <- ddply(dfm, .(segment), transform, ymax = cumsum(percentage))
dfm1 <- ddply(dfm1, .(segment), transform,ymin = ymax - percentage)
dfm1$xtext <- with(dfm1, xmin + (xmax - xmin)/2)
dfm1$ytext <- with(dfm1, ymin + (ymax - ymin)/2)

# join() function, connects two tables data.frame
dfm2<-join(melt_df, dfm1, by = c("segment", "variable"), type = "left", match = "all")

# View the final merged dataset
head(dfm2)

# Create visualization
# Basic Plot
p <- ggplot() +
  geom_rect(aes(ymin = ymin, ymax = ymax, xmin = xmin, xmax = xmax, fill = variable),dfm2,colour = "black") +
  geom_text(aes(x = xtext, y = ytext,  label = value),dfm2 ,size = 4)+
  geom_text(aes(x = xtext, y = 103, label = paste(segment)),dfm2 ,size = 4)+
# ... (see full tutorial for more)
```

## Key Parameters
- `fill`: Maps `variable` to the fill aesthetic
- `x`: Maps `116` to the x aesthetic
- `y`: Maps `seq` to the y aesthetic
- `position`: Position adjustment (identity, dodge, stack, fill)
- `stat`: Statistical transformation to use

## Tips
- The tutorial includes a '2. Advanced Plot' section with advanced styling options
- Adjust text size with `theme(text = element_text(size = 14))` for presentations
- Follow CONSORT or STROBE guidelines for clinical data visualization where applicable

## Full Tutorial
https://openbiox.github.io/Bizard/Clinics/MosaicPlot.html
