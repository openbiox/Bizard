# Skill: Stacked Area Chart (R)

## Category
DataOverTime

## When to use
Stacked area charts are similar to basic area charts, except that each dataset in the chart starts from the previous dataset and is used to show the trend line of how the size of each value changes over time or category, demonstrating the relationship between the part and the whole.

## Required R packages
- babynames
- dplyr
- ggplot2
- hrbrthemes
- plotly
- tidyverse
- viridis

## Minimal reproducible code
```r
# Basic stacked area diagram----
options(scipen = 20) # Do not use scientific notation
p <- ggplot(data_WorldPhones, aes(x=year, y=Phones, fill=area)) + 
  geom_area()+
  scale_y_continuous(limits = c(0, 150000),
                     breaks = seq(0, 150000,50000))

p
```

## Full tutorial
https://openbiox.github.io/Bizard/DataOverTime/StackedArea.html
