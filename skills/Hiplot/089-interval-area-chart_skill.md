# Skill: Interval Area Chart (R)

## Category
Hiplot

## When to Use
Create a Interval Area Chart using R with the Hiplot platform's approach. Suitable for biomedical data visualization with publication-quality output.

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
data <- data.table::fread(jsonlite::read_json("https://hiplot.cn/ui/basic/interval-area-chart/data.json")$exampleData$textarea[[1]])
data <- as.data.frame(data)

# Convert data structure
data[["month"]] <- factor(data[["month"]], levels = data[["month"]])

# View data
head(data)

# Create visualization
# Interval Area Chart
p <- ggplot(data, aes(x = month, group = 1)) +
  geom_line(aes(y = max_temperature), size = 1.2, color = "#EA3323", 
            linetype = "solid") +
  geom_line(aes(y = min_temperature), size = 1.2, color = "#0000F5", 
            linetype = "solid") +
  geom_line(aes(x = month, y = mean), size = 1.2, color = "#BEBEBE", 
            linetype = "dashed") +
  geom_ribbon(aes(ymin = min_temperature, ymax = max_temperature), 
              fill = "#F2F2F2", alpha = 0.5) +
  geom_text(aes(x = month, y = max_temperature + 1, label = max_temperature),
            color = "#EA3323", size = 2.5, vjust = -0.5, hjust = 0) +
  geom_text(aes(x = month, y = min_temperature - 1, label = min_temperature),
            color = "#0000F5", size = 2.5, vjust = 1.5, hjust = 0) +
  geom_text(aes(x = month, y = mean, label = mean),
            color = "#BEBEBE", size = 2.5, vjust = 1.5, hjust = 0) +
  labs(title = "Temperature", x = "Month", y = "Temperature") +
  scale_color_manual(values = c(max = "#EA3323", min = "#0000F5")) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

p
```

## Key Parameters
- `x`: Maps `month` to the x aesthetic
- `group`: Maps `1` to the group aesthetic
- `y`: Maps `mean` to the y aesthetic
- `alpha`: Controls transparency (0 = fully transparent, 1 = opaque)
- `theme`: Plot theme; tutorial uses `theme_bw()`

## Tips
- Use `theme_minimal()` or `theme_bw()` for clean, publication-ready plots
- Customize color scales with `scale_fill_manual()` or `scale_color_brewer()`
- See the full tutorial for additional customization options and advanced examples

## Full Tutorial
https://openbiox.github.io/Bizard/Hiplot/089-interval-area-chart.html
