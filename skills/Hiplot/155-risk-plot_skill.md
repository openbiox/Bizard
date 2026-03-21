# Skill: Risk Factor Analysis (R)

## Category
Hiplot

## When to Use
Create a Risk Factor Analysis using R with the Hiplot platform's approach. Suitable for biomedical data visualization with publication-quality output.

## Required R Packages
- cowplot
- cutoff
- data.table
- fastStat
- ggplot2
- jsonlite
- survminer

## Minimal Reproducible Code
```r
# Load packages
library(cowplot)
library(cutoff)
library(data.table)
library(fastStat)
library(ggplot2)
library(jsonlite)

# Prepare data
# Load data
data <- data.table::fread(jsonlite::read_json("https://hiplot.cn/ui/basic/risk-plot/data.json")$exampleData$textarea[[1]])
data <- as.data.frame(data)

# Convert data structure
data <- data[order(data[, "riskscore"], decreasing = F), ]
cutoff_point <- median(x = data$riskscore, na.rm = TRUE)
data$Group <- ifelse(data$riskscore > cutoff_point, "High", "Low")
cut.position <- (1:nrow(data))[data$riskscore == cutoff_point]
if (length(cut.position) == 0) {
  cut.position <- which.min(abs(data$riskscore - cutoff_point))
} else if (length(cut.position) > 1) {
  cut.position <- cut.position[length(cut.position)]
}
## Generate the data.frame required to draw A B graph
data2 <- data[, c("time", "event", "riskscore", "Group")]

# View data
head(data)

# Create visualization
# Risk Factor Analysis
## Figure A
fA <- ggplot(data = data2, aes(x = 1:nrow(data2), y = data2$riskscore, 
                               color = Group)) +
  geom_point(size = 2) +
  scale_color_manual(name = "Risk Group", 
                     values = c("Low" = "#0B45A5", "High" = "#E20B0B")) +
  geom_vline(xintercept = cut.position, linetype = "dotted", size = 1) +
  theme(panel.grid = element_blank(), panel.background = element_blank(),
        axis.ticks.x = element_blank(), axis.line.x = element_blank(),
        axis.text.x = element_blank(), axis.title.x = element_blank(),
        axis.title.y = element_text(size = 14, vjust = 1, angle = 90),
        axis.text.y = element_text(size = 11),
        axis.line.y = element_line(size = 0.5, colour = "black"),
        axis.ticks.y = element_line(size = 0.5, colour = "black"),
        legend.title = element_text(size = 13), 
        legend.text = element_text(size = 12)) +
  coord_trans() +
  ylab("Risk Score") +
  scale_x_continuous(expand = c(0, 3))
# ... (see full tutorial for more)
```

## Key Parameters
- `x`: Maps `id` to the x aesthetic
- `fill`: Maps `value` to the fill aesthetic
- `y`: Maps `variable` to the y aesthetic
- `position`: Position adjustment (identity, dodge, stack, fill)

## Tips
- Customize color scales with `scale_fill_manual()` or `scale_color_brewer()`
- Adjust text size with `theme(text = element_text(size = 14))` for presentations
- See the full tutorial for additional customization options and advanced examples

## Full Tutorial
https://openbiox.github.io/Bizard/Hiplot/155-risk-plot.html
