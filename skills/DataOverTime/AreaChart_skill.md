# Skill: Area Chart (R)

## Category
DataOverTime

## When to Use
An area chart is a line chart in which the area below the line is filled with color. It is mainly used to display values at continuous intervals or over a time span.

## Required R Packages
- dygraphs
- ggpattern
- hrbrthemes
- tidyverse
- viridis
- xts

## Minimal Reproducible Code
```r
# Load packages
library(dygraphs)
library(ggpattern)
library(hrbrthemes)
library(tidyverse)
library(viridis)
library(xts)

# Prepare data
# TCGA-BRCA.survival.tsv
tcga_brca_survival <- readr::read_tsv("https://bizard-1301043367.cos.ap-guangzhou.myqcloud.com/TCGA-BRCA.survival.tsv")

tcga_brca_filtered <- tcga_brca_survival %>%
  filter(OS.time <= 2000) %>%
  mutate(month = floor(OS.time / 30))

monthly_death_counts <- tcga_brca_filtered %>%
  filter(OS == 1) %>%
  group_by(month) %>%
  summarise(deaths = n())

# AirPassengers
data("AirPassengers")
air_passenger_data <- as.data.frame(AirPassengers)

air_passenger_data$Month <- rep(month.name, 12)
air_passenger_data$Year <- rep(1949:1960, each=12)
air_passenger_data$x <- as.numeric(air_passenger_data$x)

air_passenger_long <- air_passenger_data %>%
  gather(key = "Variable", value = "Value", -Year, -Month)

air_passenger_percentage <- air_passenger_long %>%
  group_by(Year) %>%
  mutate(Percentage = Value / sum(Value) * 100) # Calculate the percentage for each month

air_passenger_time_series <- data.frame(datetime = time(AirPassengers), count = as.vector(AirPassengers))

air_passenger_time_series$datetime <- as.Date(air_passenger_time_series$datetime)
air_passenger_xts <- xts(x = air_passenger_time_series$count, order.by = air_passenger_time_series$datetime) # Creating an XTS object

# Create visualization
# Basic area plot
p <- ggplot(monthly_death_counts, aes(x = month, y = deaths)) +
  geom_area() +
  labs(title = "Cumulative Deaths Over Time",
       x = "Months",
       y = "Number of Deaths")

p
```

## Key Parameters
- `x`: Maps `Year` to the x aesthetic
- `y`: Maps `Percentage` to the y aesthetic
- `fill`: Maps `Month` to the fill aesthetic
- `alpha`: Controls transparency (0 = fully transparent, 1 = opaque)
- `width`: Controls element width
- `theme`: Plot theme; tutorial uses `theme_ipsum()`

## Tips
- Use `theme_minimal()` or `theme_bw()` for clean, publication-ready plots
- Customize color scales with `scale_fill_manual()` or `scale_color_brewer()`
- Highlight key time points or events with vertical reference lines or annotations

## Full Tutorial
https://openbiox.github.io/Bizard/DataOverTime/AreaChart.html
