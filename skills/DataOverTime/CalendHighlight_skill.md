# Skill: Calend Highlight (R)

## Category
DataOverTime

## When to Use
Date highlighting marks are mainly used to display changes in data within certain specific date ranges in time series data, and can be used for an overview of activity frequencies and marking of special dates.

## Required R Packages
- calendR

## Minimal Reproducible Code
```r
# Load packages
library(calendR)

# Prepare data
# Construct data
set.seed(1)
data <- rnorm(365)

# View data
head(data)

# Create visualization
# Chinese Calendar
p <- calendR(
	year = 2025,
	month = NULL,
	from = NULL,
	to = NULL,
	start = "M",
	mbg.col = 2,
	# orientation = "portrait",
	months.col = "white",
	months.pos = 0.5,
	monthnames = c(
		"一月",
		"二月",
		"三月",
		"四月",
		"五月",
		"六月",
		"七月",
		"八月",
		"九月",
		"十月",
		"十一月",
		"十二月"
	),
	weeknames = c("一", "二", "三", "四", "五", "六", "日"),
	special.days = data,
	special.col = "#00338888",
	gradient = TRUE,
	low.col = "#FFFFFF88",
	font.family = "sans",
	font.style = "plain",
	day.size = 2,
	# ncol = 2,
	lunar = FALSE,
	pdf = FALSE
)

# ... (see full tutorial for more)
```

## Key Parameters
- `fill`: Maps a variable to fill color for group comparison
- `color`: Maps a variable to outline/point color

## Tips
- Adjust text size with `theme(text = element_text(size = 14))` for presentations
- Highlight key time points or events with vertical reference lines or annotations
- See the full tutorial for additional customization options and advanced examples

## Full Tutorial
https://openbiox.github.io/Bizard/DataOverTime/CalendHighlight.html
