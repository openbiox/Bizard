# Skill: Lollipop Plot (R)

## Category
Clinics

## When to use
Visualize lollipop plot data in a biomedical context.

## Required R packages
- dplyr
- ggplot2
- ggpubr
- patchwork

## Minimal reproducible code
```r
# Basic Lollipop Plot
# Convert correlation coefficients and p-values to categorical variables
data$pvalue_group <- cut(data$pvalue,
                         breaks = c(0, 0.2, 0.4, 0.6,0.8, 1),
                         labels = c("< 0.2","< 0.4","< 0.6","< 0.8","<1"),
                         right=FALSE)# right=FALSE表示表示区间为左闭右开
data$cor_group_size <- cut(abs(data$cor),# 绝对值
                      breaks = c(0, 0.1, 0.2, 0.3, 0.4, 0.5),
                      labels = c("0.1","0.2","0.3","0.4","0.5"),
                      right=FALSE) 
# Order
data = data[order(data$cor),]
data$cell = factor(data$cell, levels = data$cell)

p = ggplot(data, 
           aes(x = cor, y = cell, color = pvalue_group)) +
  scale_color_manual(name="pvalue",
                     values = c("#146432", 
                                "#4DB748", 
                                #"#FAA519", # Since there is no data in this interval, comment it out.
                                "#FABECD" #,
                                #"#FAD700" #Since there is no data in this interval, comment it out.
                                ))+ # Color selection of candies in lollipops
  geom_segment(aes(x = 0, y = cell, xend = cor, yend = cell),
               color = 'black', # Drawing of the stick in a lollipop
               linewidth = 0.5) +
  geom_point(aes(size = cor_group_size))+ # Drawing of candy in lollipop
  labs(title = "COL17A1", # Image title
       size = "abs(cor)") + # legend name
  guides(color = "none")+ # Hide redundant legends
  theme_bw()+ 
  theme(plot.title=element_text(size=8,  # title size
                                hjust=0.5 ), # title position
        legend.position = "bottom", # legend position
        text = element_text(family = "serif"), # Set the font to Times New Roman
        panel.grid = element_line(linetype = "dotted",color='grey')) 
p
```

## Full tutorial
https://openbiox.github.io/Bizard/Clinics/LollipopPlot.html
