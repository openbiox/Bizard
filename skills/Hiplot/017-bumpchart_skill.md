# Skill: Bumpchart (R)

## Category
Hiplot

## When to Use
Bump chart can be used to display the change of grouped values.

## Required R Packages
- data.table
- dplyr
- ggbump
- ggplot2
- jsonlite

## Minimal Reproducible Code
```r
# Load packages
library(data.table)
library(dplyr)
library(ggbump)
library(ggplot2)
library(jsonlite)

# Prepare data
# Load data
data <- data.table::fread(jsonlite::read_json("https://hiplot.cn/ui/basic/bumpchart/data.json")$exampleData$textarea[[1]])
data <- as.data.frame(data)

# View data
head(data)

# Create visualization
# Bumpchart
p <- ggplot(data, aes(x = x, y = y, color = group)) +
  geom_bump(size = 1.5) +
  geom_point(size = 5) +
  geom_text(data = data %>% filter(x == min(x)),
            aes(x = x - 0.1, label = group),
            size = 5, hjust = 1) +
  geom_text(data = data %>% filter(x == max(x)),
            aes(x = x + 0.1, label = group),
            size = 5, hjust = 0) +
  theme_void() +
  theme(legend.position = "none") +
  scale_color_manual(values = c("#0571B0","#92C5DE","#F4A582","#CA0020"))

p
```

## Key Parameters
- `x`: Maps `x` to the x aesthetic
- `y`: Maps `y` to the y aesthetic
- `color`: Maps `group` to the color aesthetic
- `position`: Position adjustment (identity, dodge, stack, fill)
- `theme`: Plot theme; tutorial uses `theme_void()`

## Tips
- Customize color scales with `scale_fill_manual()` or `scale_color_brewer()`
- Adjust text size with `theme(text = element_text(size = 14))` for presentations
- See the full tutorial for additional customization options and advanced examples

## Full Tutorial
https://openbiox.github.io/Bizard/Hiplot/017-bumpchart.html
