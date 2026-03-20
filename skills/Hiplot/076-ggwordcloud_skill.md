# Skill: ggwordcloud (R)

## Category
Hiplot

## When to use
::: callout-note
**Hiplot website**

## Required R packages
- curl
- data.table
- ggwordcloud
- jsonlite
- png

## Minimal reproducible code
```r
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

## Full tutorial
https://openbiox.github.io/Bizard/Hiplot/076-ggwordcloud.html
