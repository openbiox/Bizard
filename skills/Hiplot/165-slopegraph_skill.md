# Skill: Slopegraph (R)

## Category
Hiplot

## When to Use
Sopegraph can be used to display the change of values.

## Required R Packages
- CGPfunctions
- data.table
- ggplot2
- jsonlite

## Minimal Reproducible Code
```r
# Load packages
library(CGPfunctions)
library(data.table)
library(ggplot2)
library(jsonlite)

# Prepare data
# Load data
data <- data.table::fread(jsonlite::read_json("https://hiplot.cn/ui/basic/slopegraph/data.json")$exampleData$textarea[[1]])
data <- as.data.frame(data)

# convert data structure
data[, "country"] <- factor(data[ ,"country"], levels = unique(data[ ,"country"]))
data[, "year"] <- factor(data[ ,"year"], levels = unique(data[ ,"year"]))

# View data
head(data)

# Create visualization
# Slopegraph
p <- newggslopegraph(data, year, lifeExp, country) +
  labs(subtitle = "", title = "Slope Graph", x = "Life Expectancy (years)",
       y = "country", caption = "") +
  scale_color_manual(values = c("#3B4992FF", "#EE0000FF", "#008B45FF",
                               "#631879FF", "#008280FF", "#BB0021FF")) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

p
```

## Key Parameters
- `theme`: Plot theme; tutorial uses `theme_minimal()`
- `fill`: Maps a variable to fill color for group comparison
- `color`: Maps a variable to outline/point color

## Tips
- Use `theme_minimal()` or `theme_bw()` for clean, publication-ready plots
- Customize color scales with `scale_fill_manual()` or `scale_color_brewer()`
- See the full tutorial for additional customization options and advanced examples

## Full Tutorial
https://openbiox.github.io/Bizard/Hiplot/165-slopegraph.html
