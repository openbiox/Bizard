# Skill: Wordcloud (R)

## Category
Ranking

## When to use
A word cloud is a visual representation of text words, which allows you to clearly see the keywords (high-frequency words) in a large amount of text data.

## Required R packages
- dplyr
- htmlwidgets
- jiebaR
- jiebaRD
- tidyverse
- webshot2
- wordcloud2

## Minimal reproducible code
```r
# Basic Plotting
BasicPlot <- wordcloud2(data = words_seg, size = 1)
BasicPlot
```

## Full tutorial
https://openbiox.github.io/Bizard/Ranking/Wordcloud.html
