# Skill: ggwordcloud (R)

## Category
Hiplot

## When to Use
The word cloud is to visualize the "keywords" that appear frequently in the web text by forming a "keyword cloud layer" or "keyword rendering".

## Required R Packages
- curl
- data.table
- ggwordcloud
- jsonlite
- png

## Minimal Reproducible Code
```r
# Load packages
library(curl)
library(data.table)
library(ggwordcloud)
library(jsonlite)
library(png)

# Prepare data
# Load data
data <- data.table::fread(jsonlite::read_json("https://hiplot.cn/ui/basic/ggwordcloud/data.json")$exampleData$textarea[[1]])
data <- as.data.frame(data)
inmask <- "https://download.hiplot.cn/api/file/fetch/?path=public/demo/ggwordcloud/hearth.png"

# Convert data structure
col <- data[, 2]
data <- cbind(data, col)

# View data
head(data)

# Create visualization
# ggwordcloud
p <- ggplot(data, aes(label = word, size = freq, color = col)) +
  scale_size_area(max_size = 40) +
  theme_minimal() + 
  geom_text_wordcloud_area(
    mask = png::readPNG(curl::curl_fetch_memory(inmask)$content), 
    rm_outside = TRUE) +
  scale_color_gradient(low = "#8B0000", high = "#FF0000")

p
```

## Key Parameters
- `size`: Maps `freq` to the size aesthetic
- `color`: Maps `col` to the color aesthetic
- `theme`: Plot theme; tutorial uses `theme_minimal()`

## Tips
- Use `theme_minimal()` or `theme_bw()` for clean, publication-ready plots
- Customize color scales with `scale_fill_manual()` or `scale_color_brewer()`
- See the full tutorial for additional customization options and advanced examples

## Full Tutorial
https://openbiox.github.io/Bizard/Hiplot/076-ggwordcloud.html
