# Skill: Calibration Curve (R)

## Category
Hiplot

## When to Use
The calibration curve is used to evaluate the consistency / calibration, i.e. the difference between the predicted value and the real value.

## Required R Packages
- data.table
- ggplotify
- jsonlite
- rms
- survival

## Minimal Reproducible Code
```r
# Load packages
library(data.table)
library(ggplotify)
library(jsonlite)
library(rms)
library(survival)

# Prepare data
# Load data
data <- data.table::fread(jsonlite::read_json("https://hiplot.cn/ui/basic/calibration-curve/data.json")$exampleData$textarea[[1]])
data <- as.data.frame(data)

# convert data structure
res.lrm <- lrm(as.formula(paste(
  "status ~ ", 
  paste(colnames(data)[3:length(colnames(data))], collapse = "+"))),
  data = data, x = TRUE, y = TRUE)

lrm.cal <- calibrate(res.lrm, method = "boot", B = length(rownames(data)))

# View data
head(data)

# Create visualization
# Calibration Curve
p <- as.ggplot(function() {
  plot(lrm.cal,
       xlab = "Nomogram Predicted Survival",
       ylab = "Actual Survival",
       main = "Calibration Curve"
       )
})

p
```

## Key Parameters
- `stat`: Statistical transformation to use
- `fill`: Maps a variable to fill color for group comparison
- `color`: Maps a variable to outline/point color

## Tips
- Adjust text size with `theme(text = element_text(size = 14))` for presentations
- See the full tutorial for additional customization options and advanced examples

## Full Tutorial
https://openbiox.github.io/Bizard/Hiplot/018-calibration-curve.html
