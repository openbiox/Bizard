# Skill: Graph Gallery (R)

## Category
Misc

## When to use
👋 **Bizard** is a comprehensive repository of advanced visualization codes tailored for biomedical research. It currently includes approximately 750 graphics across more than 65 chart types. Below is the gallery for all graphics.

## Required R packages
- crosstalk
- dplyr
- htmltools
- jsonlite
- reactable

## Minimal reproducible code
```r
library(reactable)
library(jsonlite)
library(dplyr)
library(crosstalk)
library(htmltools)

data <- read.csv("files/gallery_data.csv")
```

## Full tutorial
https://openbiox.github.io/Bizard/GraphGallery.html
