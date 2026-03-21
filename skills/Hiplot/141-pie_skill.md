# Skill: Pie (R)

## Category
Hiplot

## When to Use
The pie chart is a statistical chart that shows the proportion of each part by dividing a circle into sections.

## Required R Packages
- data.table
- dplyr
- ggplot2
- jsonlite

## Minimal Reproducible Code
```r
# Load packages
library(data.table)
library(dplyr)
library(ggplot2)
library(jsonlite)

# Prepare data
# Load data
data <- data.table::fread(jsonlite::read_json("https://hiplot.cn/ui/basic/pie/data.json")$exampleData$textarea[[1]])
data <- as.data.frame(data)


# Convert data structure
colnames(data) <- c("Group", "Value")
data <- data %>%
  arrange(desc(Group)) %>%
  mutate(prop = Value / sum(data$Value) * 100) %>%
  mutate(ypos = Value / length(unique(Group)) +
           c(0, cumsum(Value)[-length(Value)]) + 5)

# View data
head(data)

# Create visualization
# Pie
p <- ggplot(data, aes(x = "", y = Value, fill = Group)) +
  geom_col(width = 1) +
  geom_bar(stat = "identity", width = 1, color = "white") +
  geom_text(aes(y = ypos, 
                label = sprintf("%s\n(n=%s, %s%%)", Group, Value,
                                round(Value / sum(data$Value) * 100, 2))), 
            color = "white", fontface = "bold") +
  coord_polar(theta = "y", start = 0, direction = -1) +
  guides(fill = guide_legend(title = "Group")) +
  scale_fill_discrete(
    breaks = data$Group,
    labels = paste(data$Group," (", round(data$Value / sum(data$Value) * 100, 2),
                   "%)", sep = "")) +
  scale_fill_manual(values = c("#00468BFF","#ED0000FF","#42B540FF","#0099B4FF")) +
  ggtitle("Pie Plot") + 
  theme_minimal() +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    panel.border = element_blank(),
    panel.grid = element_blank(),
    axis.ticks = element_blank(),
    plot.title = element_text(size = 14, face = "bold",
# ... (see full tutorial for more)
```

## Key Parameters
- `y`: Maps `ypos` to the y aesthetic
- `fill`: Maps `Group` to the fill aesthetic
- `width`: Controls element width
- `position`: Position adjustment (identity, dodge, stack, fill)
- `stat`: Statistical transformation to use
- `theme`: Plot theme; tutorial uses `theme_minimal()`

## Tips
- Use `theme_minimal()` or `theme_bw()` for clean, publication-ready plots
- Customize color scales with `scale_fill_manual()` or `scale_color_brewer()`
- See the full tutorial for additional customization options and advanced examples

## Full Tutorial
https://openbiox.github.io/Bizard/Hiplot/141-pie.html
