# Skill: Stacked Area Chart (R)

## Category
DataOverTime

## When to Use
Stacked area charts are similar to basic area charts, except that each dataset in the chart starts from the previous dataset and is used to show the trend line of how the size of each value changes over time or category, demonstrating the relationship between the part and the whole.

## Required R Packages
- babynames
- dplyr
- ggplot2
- hrbrthemes
- plotly
- tidyverse
- viridis

## Minimal Reproducible Code
```r
# Load packages
library(babynames)
library(dplyr)
library(ggplot2)
library(hrbrthemes)
library(plotly)
library(tidyverse)

# Prepare data
# data_WorldPhones
data_WorldPhones <- as.data.frame(WorldPhones)
data_WorldPhones <- rownames_to_column(data_WorldPhones,"year")
data_WorldPhones <- data_WorldPhones %>%
  gather(key = "area",value = "Phones",-"year" )
data_WorldPhones$year <- as.numeric(data_WorldPhones$year)

# data_USPersonalExpenditure
data_USPersonalExpenditure <- USPersonalExpenditure
data_USPersonalExpenditure <- as.data.frame(data_USPersonalExpenditure)
data_USPersonalExpenditure <- rownames_to_column(data_USPersonalExpenditure,"type")
data_USPersonalExpenditure <- data_USPersonalExpenditure %>%
  gather(key = "year",value = "expense",-"type" )
data_USPersonalExpenditure$year <- as.numeric(data_USPersonalExpenditure$year)

# data_baby
data_baby <- babynames %>% 
  filter(name %in% c("Ashley", "Amanda", "Jessica", "Patricia", "Linda", "Deborah",   "Dorothy", "Betty", "Helen")) %>%
  filter(sex=="F")

# data_covi19
data_covi19 <- readr::read_csv("https://bizard-1301043367.cos.ap-guangzhou.myqcloud.com/data_covi19.csv")
data_covi19$date <- c(6:12)
data_covi19 <- gather(data_covi19, key = "area", value = "cases", 2:7)

# Create visualization
# Basic stacked area diagram----
options(scipen = 20) # Do not use scientific notation
p <- ggplot(data_WorldPhones, aes(x=year, y=Phones, fill=area)) + 
  geom_area()+
  scale_y_continuous(limits = c(0, 150000),
                     breaks = seq(0, 150000,50000))

p
```

## Key Parameters
- `x`: Maps `year` to the x aesthetic
- `y`: Maps `n` to the y aesthetic
- `fill`: Maps `name` to the fill aesthetic
- `alpha`: Controls transparency (0 = fully transparent, 1 = opaque)
- `position`: Position adjustment (identity, dodge, stack, fill)
- `theme`: Plot theme; tutorial uses `theme_ipsum()`

## Tips
- The tutorial includes a '3. Customization' section with advanced styling options
- Customize color scales with `scale_fill_manual()` or `scale_color_brewer()`
- Highlight key time points or events with vertical reference lines or annotations

## Full Tutorial
https://openbiox.github.io/Bizard/DataOverTime/StackedArea.html
