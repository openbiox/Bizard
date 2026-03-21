# Skill: Percentsge Stacked Bar Chart (R)

## Category
Hiplot

## When to Use
Create a Percentsge Stacked Bar Chart using R with the Hiplot platform's approach. Suitable for biomedical data visualization with publication-quality output.

## Required R Packages
- data.table
- dplyr
- ggplot2
- jsonlite
- scales
- tidyr

## Minimal Reproducible Code
```r
# Load packages
library(data.table)
library(dplyr)
library(ggplot2)
library(jsonlite)
library(scales)
library(tidyr)

# Prepare data
# Load data
data <- data.table::fread(jsonlite::read_json("https://hiplot.cn/ui/basic/stacked-percentage-bar-chart/data.json")$exampleData$textarea[[1]])
data <- as.data.frame(data)

# convert data structure
data$total <- rowSums(data[, -1])
data_long <- gather(data, kinds, value, -days, -total)
data_long <- data_long %>%
  group_by(days) %>%
  mutate(percent = value / total * 100)
data_long[["days"]] <- factor(data_long[["days"]], levels = data[["days"]])

# View data
head(data)

# Create visualization
# Percentsge Stacked Bar Chart
p <- ggplot(data_long, aes(x = percent, y = days, fill = kinds)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_text(aes(label = ifelse(percent != 0, paste0(round(percent), "%"), "")),
            position = position_stack(vjust = 0.5)) +
  labs(title = "Percentage Stacked Bar Chart", x = "Percentage", y = "Days") +
  scale_x_continuous(labels = percent_format(scale = 1)) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_fill_manual(values = c("#E64B35FF","#4DBBD5FF","#00A087FF"))

p
```

## Key Parameters
- `x`: Maps `percent` to the x aesthetic
- `y`: Maps `days` to the y aesthetic
- `fill`: Maps `kinds` to the fill aesthetic
- `position`: Position adjustment (identity, dodge, stack, fill)
- `stat`: Statistical transformation to use
- `theme`: Plot theme; tutorial uses `theme_bw()`

## Tips
- Use `theme_minimal()` or `theme_bw()` for clean, publication-ready plots
- Customize color scales with `scale_fill_manual()` or `scale_color_brewer()`
- See the full tutorial for additional customization options and advanced examples

## Full Tutorial
https://openbiox.github.io/Bizard/Hiplot/167-stacked-percentage-bar-chart.html
