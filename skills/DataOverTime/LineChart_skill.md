# Skill: Line Chart (R)

## Category
DataOverTime

## When to Use
Drawing line segments in various charts is common, and this module will draw all kinds of line segments that may be used.

## Required R Packages
- dplyr
- gghighlight
- ggplot2
- ggpmisc
- patchwork
- viridis

## Minimal Reproducible Code
```r
# Load packages
library(dplyr)
library(gghighlight)
library(ggplot2)
library(ggpmisc)
library(patchwork)
library(viridis)

# Prepare data
# 1.iris data
data <- iris

head(data)

# 2.economics data
# (1) Using economics data directly to draw graphs
# (2) Processing economics data to draw time series graphs
data_economics <- economics[,c(1, 4, 5)] %>%
  filter(grepl("-12-01", date)) %>%           # Select only December data for plotting
  mutate(date = gsub("-.*", "", date)) %>%    # Only keep the year
  slice(1:25) %>%                             # Choose the first 25 years
  arrange(date)                               # Sort

head(data_economics)

# 3.Automatically generate data (for log transformation of the y-axis).
data_create <- data.frame(
  x = seq(11, 100),
  y = seq(11, 100) / 2 + rnorm(90)
)

head(data_create)

# 4.Glucose level (used to emphasize specific line segments)
data_glucose <- read.csv("https://bizard-1301043367.cos.ap-guangzhou.myqcloud.com/Dexcom_001.csv", header = T)

# Glucose value data processing
data_glucose <- data_glucose[,c(2, 8)] %>%
  slice(1:102) %>%
  setNames(c("V1", "V2")) %>%
  filter(!is.na(V2) & V1 != "") %>%     # Remove na
  mutate(V3 = rep(1:30, times = 3),     # Divided into 3 stages
         group = rep(c("stage one", "stage two", "stage three"), each = 30))

head(data_glucose)

# Create visualization
# Basic Plotting
p <- ggplot(data, aes(x = Sepal.Length, y = Sepal.Width)) +
  geom_line()
# ... (see full tutorial for more)
```

## Key Parameters
- `x`: Maps `date` to the x aesthetic
- `y`: Maps `uempmed` to the y aesthetic
- `color`: Maps `group` to the color aesthetic
- `alpha`: Controls transparency (0 = fully transparent, 1 = opaque)
- `width`: Controls element width
- `position`: Position adjustment (identity, dodge, stack, fill)
- `stat`: Statistical transformation to use

## Tips
- Customize color scales with `scale_fill_manual()` or `scale_color_brewer()`
- Adjust text size with `theme(text = element_text(size = 14))` for presentations
- Highlight key time points or events with vertical reference lines or annotations

## Full Tutorial
https://openbiox.github.io/Bizard/DataOverTime/LineChart.html
