# Skill: Seqlogo (R)

## Category
Hiplot

## When to Use
The sequence LOGO is a graphic that describes a sequence pattern of binding sites.

## Required R Packages
- data.table
- ggplot2
- ggseqlogo
- jsonlite

## Minimal Reproducible Code
```r
# Load packages
library(data.table)
library(ggplot2)
library(ggseqlogo)
library(jsonlite)

# Prepare data
# Load data
data <- data.table::fread(jsonlite::read_json("https://hiplot.cn/ui/basic/ggseqlogo/data.json")$exampleData$textarea[[1]])
data <- as.data.frame(data)

# Convert data structure
data <- data[, !sapply(data, function(x) {all(is.na(x))})]
data <- as.list(data)
data <- lapply(data, function(x) {return(x[!is.na(x)])})

# View data
str(data[1:5])

# Create visualization
# Seqlogo
p <- ggseqlogo(
  data,
  ncol = 4,
  col_scheme = "nucleotide",
  seq_type = "dna",
  method = "bits") + 
  theme(plot.title = element_text(hjust = 0.5))

p
```

## Key Parameters
- `fill`: Maps a variable to fill color for group comparison
- `color`: Maps a variable to outline/point color

## Tips
- Adjust text size with `theme(text = element_text(size = 14))` for presentations
- See the full tutorial for additional customization options and advanced examples

## Full Tutorial
https://openbiox.github.io/Bizard/Hiplot/074-ggseqlogo.html
