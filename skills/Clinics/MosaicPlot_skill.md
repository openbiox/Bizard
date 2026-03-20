# Skill: Mosaic Plot (R)

## Category
Clinics

## When to use
Visualize mosaic plot data in a biomedical context.

## Required R packages
- dplyr
- ggplot2
- plyr
- reshape2
- tidyr
- vcd
- wesanderson

## Minimal reproducible code
```r
# Basic Plot
p <- ggplot() +
  geom_rect(aes(ymin = ymin, ymax = ymax, xmin = xmin, xmax = xmax, fill = variable),dfm2,colour = "black") +
  geom_text(aes(x = xtext, y = ytext,  label = value),dfm2 ,size = 4)+
  geom_text(aes(x = xtext, y = 103, label = paste(segment)),dfm2 ,size = 4)+
  geom_text(aes(x = 102, y = seq(12.5,100,25), label = c("Macrophage","Epithelial","T cells","B cells")), size = 4,hjust = 0)+
  scale_x_continuous(breaks=seq(0,100,25),limits=c(0,110))+
  theme(panel.background=element_rect(fill="white",colour=NA),
        panel.grid.major = element_line(colour = "grey60",size=.25,linetype ="dotted" ),
        panel.grid.minor = element_line(colour = "grey60",size=.25,linetype ="dotted" ),
        text=element_text(size=15),
        legend.position="none")

p
```

## Full tutorial
https://openbiox.github.io/Bizard/Clinics/MosaicPlot.html
