# Skill: Neural Network (R)

## Category
Hiplot

## When to Use
Create a Neural Network using R with the Hiplot platform's approach. Suitable for biomedical data visualization with publication-quality output.

## Required R Packages
- NeuralNetTools
- data.table
- jsonlite
- nnet

## Minimal Reproducible Code
```r
# Load packages
library(NeuralNetTools)
library(data.table)
library(jsonlite)
library(nnet)

# Prepare data
# Load data
data <- data.table::fread(jsonlite::read_json("https://hiplot.cn/ui/basic/neural-network/data.json")$exampleData$textarea[[1]])
data <- as.data.frame(data)

# View data
head(data)

# Create visualization
# Neural Network
mod <- nnet(Y1 ~ X1 + X2 + X3, data = neuraldat, size = 10,
            maxint = 100, decay = 0)

# plot
par(mar = numeric(4))
plotnet(mod)
```

## Key Parameters
- `fill`: Maps a variable to fill color for group comparison
- `color`: Maps a variable to outline/point color

## Tips
- Adjust text size with `theme(text = element_text(size = 14))` for presentations
- See the full tutorial for additional customization options and advanced examples

## Full Tutorial
https://openbiox.github.io/Bizard/Hiplot/129-neural-network.html
