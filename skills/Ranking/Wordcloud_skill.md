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
# Load packages
library(dplyr)
library(htmlwidgets)
library(jiebaR)
library(jiebaRD)
library(tidyverse)
library(webshot2)

# Prepare data
# 1.Chinese abstract text
words <- read.csv("https://bizard-1301043367.cos.ap-guangzhou.myqcloud.com/words.txt",header = FALSE,sep="\n")
words <- as.character(words)

head_words <- substr(words, start = 1, stop = 20)

head_words

# 2.demoFreq dataset
data <- demoFreq

head(data)

# 3.English abstract text
words_english <- read.csv("https://bizard-1301043367.cos.ap-guangzhou.myqcloud.com/words_english.txt",header = FALSE,sep="\n")

words_english <- as.character(words_english)

head_words_english <- substr(words_english, start = 1, stop = 20)

head_words_english

# Create visualization
# Basic Plotting
BasicPlot <- wordcloud2(data = words_seg, size = 1)
BasicPlot
```

## Key Parameters
- `fill`: Maps a variable to fill color for group comparison
- `color`: Maps a variable to outline/point color

## Tips
- Adjust text size with `theme(text = element_text(size = 14))` for presentations
- Sort categories by value rather than alphabetically for clearer ranking visualization
- See the full tutorial for additional customization options and advanced examples

## Full Tutorial
https://openbiox.github.io/Bizard/Ranking/Wordcloud.html
