# Skill: Wordcloud (R)

## Category
Ranking

## When to Use
A word cloud is a visual representation of text words, which allows you to clearly see the keywords (high-frequency words) in a large amount of text data.

## Required R Packages
- dplyr
- htmlwidgets
- jiebaR
- jiebaRD
- tidyverse
- webshot2
- wordcloud2

## Minimal Reproducible Code
```r
# Basic Plotting
BasicPlot <- wordcloud2(data = words_seg, size = 1)
BasicPlot
```

## Full Tutorial
https://openbiox.github.io/Bizard/Ranking/Wordcloud.html
