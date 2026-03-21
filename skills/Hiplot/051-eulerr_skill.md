# Skill: Eulerr Plot (R)

## Category
Hiplot

## When to Use
Create a Eulerr Plot using R with the Hiplot platform's approach. Suitable for biomedical data visualization with publication-quality output.

## Required R Packages
- data.table
- eulerr
- ggplotify
- jsonlite

## Minimal Reproducible Code
```r
# Load packages
library(data.table)
library(eulerr)
library(ggplotify)
library(jsonlite)

# Prepare data
# Load data
data <- data.table::fread(jsonlite::read_json("https://hiplot.cn/ui/basic/eulerr/data.json")$exampleData$textarea[[1]])
data <- as.data.frame(data)

# convert data structure
genes <- as.numeric(data[, 2])
names(genes) <- as.character(data[, 1])
euler_set <- euler(genes)
  
# View data
head(data)

# Create visualization
# Eulerr Plot
fill <- c("#3B4992FF","#EE0000FF","#008B45FF","#631879FF","#008280FF","#BB0021FF",
          "#5F559BFF","#A20056FF")
p <- as.ggplot(
  plot(euler_set,
    labels = list(col = rep("white", length(genes))),
    fills = list(fill = fill),
    quantities = list(type = c("percent", "counts"),
    col = rep("white", length(genes))),
    main = "Eulerr")
)

p
```

## Key Parameters
- `fill`: Maps a variable to fill color for group comparison
- `color`: Maps a variable to outline/point color

## Tips
- Adjust text size with `theme(text = element_text(size = 14))` for presentations
- See the full tutorial for additional customization options and advanced examples

## Full Tutorial
https://openbiox.github.io/Bizard/Hiplot/051-eulerr.html
