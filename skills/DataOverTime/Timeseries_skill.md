# Skill: Timeseries (R)

## Category
DataOverTime

## When to Use
A time series graph is a statistical chart with time on the horizontal axis and the observed variable on the vertical axis, reflecting the trend of the observed variable over time.

## Required R Packages
- dplyr
- ggplot2
- patchwork

## Minimal Reproducible Code
```r
# Load packages
library(dplyr)
library(ggplot2)
library(patchwork)

# Prepare data
# 1.economics dataset
data <- economics[1:60, c(1, 4)]

head(data)

data_double <- economics[1:60, c(1, 4, 5)]   # This data is used for subplot merging and dual y-axis.

head(data_double)

# 2.Quantitative dehydration estimation data
data_water <- read.csv("https://bizard-1301043367.cos.ap-guangzhou.myqcloud.com/dehydration_estimation.csv", header = T)

axis_name <- colnames(data_water)[c(5, 8)]        # Record column names
data_water <- data_water %>%                      # Select 2 sets of data
  slice(c(19:27, 46:54)) %>%
  select(c(1, 5, 8)) %>%        
  setNames(c("V1", "V2", "V3")) %>%               # Change column name
  mutate(V4 = case_when(V1 == 3 ~ "people1",      # Column V4 serves as category labels.
                        V1 == 6 ~ "people2"))

head(data_water)

# Create visualization
# Basic plot
p <- ggplot(data, aes(x = date, y = psavert)) +
  geom_line() +
  xlab("")

p
```

## Key Parameters
- `x`: Maps `V2` to the x aesthetic
- `y`: Maps `V3` to the y aesthetic
- `group`: Maps `V4` to the group aesthetic
- `color`: Maps `V4` to the color aesthetic
- `position`: Position adjustment (identity, dodge, stack, fill)

## Tips
- Adjust text size with `theme(text = element_text(size = 14))` for presentations
- Highlight key time points or events with vertical reference lines or annotations
- See the full tutorial for additional customization options and advanced examples

## Full Tutorial
https://openbiox.github.io/Bizard/DataOverTime/Timeseries.html
