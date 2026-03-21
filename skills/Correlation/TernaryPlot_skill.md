# Skill: Ternary chart (R)

## Category
Correlation

## When to Use
A ternary chart is a type of chart used to display the proportional relationship between three variables. These three variables typically represent a certain component (such as chemical composition, species ratio, nutritional structure, etc.), and their sum is a constant, with the most common being 1 or 100%. A ternary chart uses an equilateral triangle to represent the proportional relationship between these three variables, with each point's position reflecting the relative proportion of th...

## Required R Packages
- ggtern
- ggthemes

## Minimal Reproducible Code
```r
# Load packages
library(ggtern)
library(ggthemes)

# Prepare data
# Load data
data <- read.table("https://bizard-1301043367.cos.ap-guangzhou.myqcloud.com/Tern_data.txt", header=T, row.names=1, sep="\t", comment.char = "")
# View data
head(data[,1:5])

# Create visualization
# Basic Ternary Chart
p1_1 <- ggtern(data=data, aes(x=CK, y=NPK, z=NPKM)) +
  geom_mask() +
  geom_point(aes(size=size,color=Genus),alpha=0.8)
p1_1
```

## Key Parameters
- `x`: Maps `CK` to the x aesthetic
- `y`: Maps `NPK` to the y aesthetic
- `size`: Maps `size` to the size aesthetic
- `color`: Maps `Genus` to the color aesthetic
- `alpha`: Controls transparency (0 = fully transparent, 1 = opaque)
- `theme`: Plot theme; tutorial uses `theme_tropical()`

## Tips
- The tutorial includes a '3. Beautify plots' section with advanced styling options
- Use `theme_minimal()` or `theme_bw()` for clean, publication-ready plots
- Customize color scales with `scale_fill_manual()` or `scale_color_brewer()`
- Always check and report the correlation coefficient and p-value alongside visual patterns

## Full Tutorial
https://openbiox.github.io/Bizard/Correlation/TernaryPlot.html
