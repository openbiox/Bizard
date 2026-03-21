# Skill: Part highlights the pie chart (R)

## Category
Composition

## When to Use
Create a Part highlights the pie chart visualization in R for biomedical data analysis and research publications.

## Required R Packages
- dplyr
- ggforce
- ggplot2
- ggpubr
- patchwork
- plotrix

## Minimal Reproducible Code
```r
# Load packages
library(dplyr)
library(ggforce)
library(ggplot2)
library(ggpubr)
library(patchwork)
library(plotrix)

# Prepare data
# Generate simulated data
count.data <- data.frame(
  class = c("1st", "2nd", "3rd", "Crew"),
  n = c(325, 285, 706, 885),
  prop = c(14.8, 12.9, 32.1, 40.2)
)
# Add label position
count.data <- count.data %>%
  arrange(desc(class)) %>%
  mutate(lab.ypos = cumsum(prop) - 0.5*prop)
# View the final merged dataset
head(count.data)

# Create visualization
# Basic pie chart
mycols <- c("#0073C2FF", "#EFC000FF", "#868686FF", "#CD534CFF")
p <- 
  ggplot(count.data, aes(x = "", y = prop, fill = class)) +
  geom_bar(width = 1, stat = "identity", color = "white") +
  coord_polar("y", start = 0)+
  geom_text(aes(y = lab.ypos, label = prop), color = "white")+
  scale_fill_manual(values = mycols) +
  theme_void()

p
```

## Key Parameters
- `y`: Maps `lab` to the y aesthetic
- `fill`: Maps `gene` to the fill aesthetic
- `x`: Maps `2` to the x aesthetic
- `color`: Maps `gene` to the color aesthetic
- `width`: Controls element width
- `position`: Position adjustment (identity, dodge, stack, fill)
- `stat`: Statistical transformation to use
- `theme`: Plot theme; tutorial uses `theme_void()`

## Tips
- The tutorial includes a '2. Advanced Plot' section with advanced styling options
- Customize color scales with `scale_fill_manual()` or `scale_color_brewer()`
- Ensure proportions sum to 100% and consider using a colorblind-friendly palette

## Full Tutorial
https://openbiox.github.io/Bizard/Composition/PartPieChart.html
