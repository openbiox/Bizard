# Skill: Lollipop Plot (R)

## Category
Clinics

## When to Use
Create a Lollipop Plot visualization in R for biomedical data analysis and research publications.

## Required R Packages
- dplyr
- ggplot2
- ggpubr
- patchwork

## Minimal Reproducible Code
```r
# Load packages
library(dplyr)
library(ggplot2)
library(ggpubr)
library(patchwork)

# Prepare data
# Loading data
data <- read.csv('https://bizard-1301043367.cos.ap-guangzhou.myqcloud.com/lollipop_1.csv', row.names = 1) # Correlation analysis data reading
# View the dataset
head(data)

# Create visualization
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

## Key Parameters
- `x`: Maps `0` to the x aesthetic
- `y`: Maps `cell` to the y aesthetic
- `color`: Maps `pvalue_group` to the color aesthetic
- `size`: Maps `cor_group_size` to the size aesthetic
- `width`: Controls element width
- `position`: Position adjustment (identity, dodge, stack, fill)
- `stat`: Statistical transformation to use
- `theme`: Plot theme; tutorial uses `theme_bw()`

## Tips
- The tutorial includes a '3. Beautify Plot' section with advanced styling options
- Use `theme_minimal()` or `theme_bw()` for clean, publication-ready plots
- Customize color scales with `scale_fill_manual()` or `scale_color_brewer()`
- Follow CONSORT or STROBE guidelines for clinical data visualization where applicable

## Full Tutorial
https://openbiox.github.io/Bizard/Clinics/LollipopPlot.html
