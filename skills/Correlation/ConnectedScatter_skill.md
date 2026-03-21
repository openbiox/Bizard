# Skill: Connected Scatter (R)

## Category
Correlation

## When to Use
Connected scatter is a type of chart that builds upon scatter by adding lines to connect the data points in a certain order. It allows us to discern not only the correlation between independent variable and dependent variable but also the trend in the data points.

## Required R Packages
- dplyr
- ggplot2
- stringr

## Minimal Reproducible Code
```r
# Load packages
library(dplyr)
library(ggplot2)
library(stringr)

# Prepare data
# 1.Load iris data
data("iris", package = "datasets")
data <- iris

# 2. Load and filter time series data
# To simplify the plotting process, only the data from December is selected to represent the entire year
data_economics <- economics %>%
  select(date, psavert, uempmed) %>%
  filter(str_detect(date, "-12-01")) %>%
  slice_head(n = 25) %>%
  mutate(date = str_extract(date, "^\\d{4}")) %>% #extracts the 4-digit year from the beginning of each date string.
  select(date, psavert, uempmed)

# 3.Load gene expression data (first two rows)
data_counts <- read.csv("https://bizard-1301043367.cos.ap-guangzhou.myqcloud.com/GSE243555_all_genes_with_counts.txt", sep = "\t", header = TRUE, nrows = 10)

axis_names <- data_counts[c(1, 2), 1]        # Save names

data_counts <- data_counts %>%      
  select(-1) %>%               # Remove first column
  slice(1:2) %>%               # remain the first two rows
  t()  %>%                     # Transpose
  as.data.frame() %>%
  setNames(c("V1", "V2"))      # Set column names

head(data_counts)

# Create visualization
# Basic plotting, only adding `geom_line`
p <- ggplot(data[data$Species == "setosa", ], aes(x = Sepal.Width, y = Sepal.Length)) +
  geom_point(shape = 17, size = 1.5, color = "blue") +
  geom_line()

p
```

## Key Parameters
- `x`: Maps `Sepal` to the x aesthetic
- `y`: Maps `Sepal` to the y aesthetic
- `color`: Maps `Species` to the color aesthetic
- `shape`: Maps `Species` to the shape aesthetic
- `width`: Controls element width
- `position`: Position adjustment (identity, dodge, stack, fill)

## Tips
- Adjust text size with `theme(text = element_text(size = 14))` for presentations
- Always check and report the correlation coefficient and p-value alongside visual patterns
- See the full tutorial for additional customization options and advanced examples

## Full Tutorial
https://openbiox.github.io/Bizard/Correlation/ConnectedScatter.html
