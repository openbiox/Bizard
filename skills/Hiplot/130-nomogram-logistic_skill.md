# Skill: Nomogram (Logistic) (R)

## Category
Hiplot

## When to Use
Create a Nomogram (Logistic) using R with the Hiplot platform's approach. Suitable for biomedical data visualization with publication-quality output.

## Required R Packages
- data.table
- ggplotify
- jsonlite
- rms

## Minimal Reproducible Code
```r
# Load packages
library(data.table)
library(ggplotify)
library(jsonlite)
library(rms)

# Prepare data
# Load data
data <- data.table::fread(jsonlite::read_json("https://hiplot.cn/ui/basic/nomogram-logistic/data.json")$exampleData$textarea[[1]])
data <- as.data.frame(data)

# Convert data structure
dd <- datadist(data)
options(datadist = "dd")
## Build Logistic model and run nomogram
logistic_res <- lrm(data=data, as.formula(paste(
    colnames(data)[1], " ~ ",
    paste(colnames(data)[2:length(colnames(data))],
      collapse = "+"
    )
  ))
)
logistic_nomo <- nomogram(logistic_res, maxscale = 100,
  fun= function(x)1/(1+exp(-x)), lp=F, funlabel="Dead Risk",
  fun.at=c(.001,.01,.05,seq(.1,.9,by=.1),.95,.99,.999)
)

# View data
head(data)

# Create visualization
# Nomogram (Logistic)
p <- as.ggplot(function() {
  plot(logistic_nomo,
    scale = 1
  )
  title(main = "Nomogram (Logistic)")
})

p
```

## Key Parameters
- `fill`: Maps a variable to fill color for group comparison
- `color`: Maps a variable to outline/point color

## Tips
- Adjust text size with `theme(text = element_text(size = 14))` for presentations
- See the full tutorial for additional customization options and advanced examples

## Full Tutorial
https://openbiox.github.io/Bizard/Hiplot/130-nomogram-logistic.html
