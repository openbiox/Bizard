# Skill: Streamgraph (R)

## Category
DataOverTime

## When to Use
A Streamgraph is a stacked area diagram. It represents the evolution of numerical variables across multiple groups. Typically, it displays areas around a central axis with rounded edges to create a flowing shape.

## Required R Packages
- dplyr
- ggplot2
- ggstream
- htmlwidgets
- streamgraph

## Minimal Reproducible Code
```r
# Load packages
library(dplyr)
library(ggplot2)
library(ggstream)
library(htmlwidgets)
library(streamgraph)

# Prepare data
# 1.R's built-in data - ChickWeight
## This dataset contains 50 samples in total. The dataset chick_new_2 below selects 5 representative samples with a Diet value of 1.
chick_new_1 <- subset(ChickWeight,Diet=="1")
chick_new_2 <- chick_new_1[c(1:12,144:155,73:95,156:167),]

# 2.Data on COVID-19 infections in 2020 (data source: GISAID database)
## The following data was obtained through data processing, where covid_all represents the total number of people infected with COVID-19 in different regions each month.
covid_all <- readr::read_csv(
"https://bizard-1301043367.cos.ap-guangzhou.myqcloud.com/covid_all.csv")
head(covid_all)
covid_month <- readr::read_csv(
"https://bizard-1301043367.cos.ap-guangzhou.myqcloud.com/covid_month.csv")
head(covid_month)

# Create visualization
streamgraph(covid_all, key = "location",
            value = "count",date = "time",
            height="300px", width="1000px")
```

## Key Parameters
- `fill`: Maps `Chick` to the fill aesthetic
- `color`: Maps `Chick` to the color aesthetic
- `width`: Controls element width

## Tips
- Adjust text size with `theme(text = element_text(size = 14))` for presentations
- Highlight key time points or events with vertical reference lines or annotations
- See the full tutorial for additional customization options and advanced examples

## Full Tutorial
https://openbiox.github.io/Bizard/DataOverTime/Streamgraph.html
