# Skill: Interval Bar Chart (R)

## Category
Hiplot

## When to Use
Create a Interval Bar Chart using R with the Hiplot platform's approach. Suitable for biomedical data visualization with publication-quality output.

## Required R Packages
- data.table
- ggplot2
- jsonlite

## Minimal Reproducible Code
```r
# Load packages
library(data.table)
library(ggplot2)
library(jsonlite)

# Prepare data
# Load data
data <- data.table::fread(jsonlite::read_json("https://hiplot.cn/ui/basic/interval-bar-chart/data.json")$exampleData$textarea[[1]])
data <- as.data.frame(data)

# Convert data structure
data$name_num <- match(data[["month"]], unique(data[["month"]]))

# View data
head(data)

# Create visualization
# Interval Bar Chart
p <- ggplot(data, aes(x = month, y = max_temperature)) +
  geom_rect(aes(xmin = name_num - 0.4, xmax = name_num + 0.4,
                ymin = min_temperature, ymax = max_temperature), 
            fill = "#282726", alpha = 0.7) +
  geom_line(aes(x = name_num, y = mean), color = "#006064", size = 0.8) +
  labs(x = "Month", y = "Temperature") +
  scale_x_discrete() +
  theme_bw()

p
```

## Key Parameters
- `x`: Maps `name_num` to the x aesthetic
- `y`: Maps `mean` to the y aesthetic
- `alpha`: Controls transparency (0 = fully transparent, 1 = opaque)
- `theme`: Plot theme; tutorial uses `theme_bw()`

## Tips
- Use `theme_minimal()` or `theme_bw()` for clean, publication-ready plots
- Adjust text size with `theme(text = element_text(size = 14))` for presentations
- See the full tutorial for additional customization options and advanced examples

## Full Tutorial
https://openbiox.github.io/Bizard/Hiplot/090-interval-bar-chart.html
