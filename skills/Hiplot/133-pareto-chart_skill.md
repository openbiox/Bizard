# Skill: Pareto Chart (R)

## Category
Hiplot

## When to Use
Create a Pareto Chart using R with the Hiplot platform's approach. Suitable for biomedical data visualization with publication-quality output.

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
data <- data.table::fread(jsonlite::read_json("https://hiplot.cn/ui/basic/pareto-chart/data.json")$exampleData$textarea[[1]])
data <- as.data.frame(data)

# Convert data structure
data <- data[order(-data[["sales"]]), ]
data[["channel"]] <- factor(data[["channel"]], levels = data[["channel"]])
## Calculate percentage number
data$accumulating <- cumsum(data[["sales"]])
max_y <- max(data[["sales"]])
cal_num <- sum(data[["sales"]]) / max_y
data$accumulating <- data$accumulating / cal_num

# View data
head(data)

# Create visualization
# Pareto Chart
p <- ggplot(data, aes(x = channel, y = sales, fill = channel)) +
  geom_bar(stat = "identity") +
  geom_line(aes(y = accumulating), group = 1) +
  geom_point(aes(y = accumulating), show.legend = FALSE) +
  scale_y_continuous(sec.axis = sec_axis(trans = ~ . / max_y * 100, name = "Percentage")) +
  scale_fill_manual(values = c("#E64B35FF","#4DBBD5FF","#00A087FF","#3C5488FF",
                               "#F39B7FFF","#8491B4FF","#91D1C2FF","#DC0000FF")) +
  theme_bw()

p
```

## Key Parameters
- `x`: Maps `channel` to the x aesthetic
- `y`: Maps `accumulating` to the y aesthetic
- `fill`: Maps `channel` to the fill aesthetic
- `stat`: Statistical transformation to use
- `theme`: Plot theme; tutorial uses `theme_bw()`

## Tips
- Use `theme_minimal()` or `theme_bw()` for clean, publication-ready plots
- Customize color scales with `scale_fill_manual()` or `scale_color_brewer()`
- See the full tutorial for additional customization options and advanced examples

## Full Tutorial
https://openbiox.github.io/Bizard/Hiplot/133-pareto-chart.html
