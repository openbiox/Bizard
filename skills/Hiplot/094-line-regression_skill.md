# Skill: Line Regression (R)

## Category
Hiplot

## When to Use
Linear regression is a regression method for linear modeling of the relationship between independent variables and dependent variables.If there is only one independent variable, it is called simple regression, and if there is more than one independent variable, it is called multiple regression.

## Required R Packages
- data.table
- ggplot2
- ggrepel
- jsonlite

## Minimal Reproducible Code
```r
# Load packages
library(data.table)
library(ggplot2)
library(ggrepel)
library(jsonlite)

# Prepare data
# Load data
data <- data.table::fread(jsonlite::read_json("https://hiplot.cn/ui/basic/line-regression/data.json")$exampleData$textarea[[1]])
data <- as.data.frame(data)

# Convert data structure
data$group <- factor(data$group, levels = unique(data$group))

# View data
head(data)

# Create visualization
# Line Regression
## Defining the equation
equation <- function(x, add_p = FALSE) {
  xs <- summary(x)
  lm_coef <- list(
    a = as.numeric(round(coef(x)[1], digits = 2)),
    b = as.numeric(round(coef(x)[2], digits = 2)),
    r2 = round(xs$r.squared, digits = 2),
    pval = xs$coef[2, 4] 
  )
  if (add_p) {
    lm_eq <- substitute(italic(y) == a + b %.% italic(x) * "," ~ ~
  italic(R)^2 ~ "=" ~ r2 * "," ~ ~ italic(p) ~ "=" ~ pval, lm_coef)
  } else {
    lm_eq <- substitute(italic(y) == a + b %.% italic(x) * "," ~ ~
  italic(R)^2 ~ "=" ~ r2, lm_coef)
  }
  as.expression(lm_eq)
}
## Plot
p <- ggplot(data, aes(x = value1, y = value2, colour = group)) +
  geom_point(show.legend = TRUE) +
  geom_smooth(method = "lm", se = T, show.legend = F) +
  geom_rug(sides = "bl", size = 1, show.legend = F) +
  scale_color_manual(values = c("#00468BFF","#ED0000FF")) +
  ggtitle("Line Reguression Plot") +
  theme_bw() +
  theme(text = element_text(family = "Arial"),
        plot.title = element_text(size = 12, hjust = 0.5),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 10),
        axis.text.x = element_text(angle = 0, hjust = 0.5,vjust = 1),
# ... (see full tutorial for more)
```

## Key Parameters
- `x`: Maps `value1` to the x aesthetic
- `y`: Maps `value2` to the y aesthetic
- `colour`: Maps `group` to the colour aesthetic
- `position`: Position adjustment (identity, dodge, stack, fill)
- `theme`: Plot theme; tutorial uses `theme_bw()`

## Tips
- Use `theme_minimal()` or `theme_bw()` for clean, publication-ready plots
- Customize color scales with `scale_fill_manual()` or `scale_color_brewer()`
- See the full tutorial for additional customization options and advanced examples

## Full Tutorial
https://openbiox.github.io/Bizard/Hiplot/094-line-regression.html
