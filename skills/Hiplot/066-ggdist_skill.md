# Skill: Dist Plot (R)

## Category
Hiplot

## When to Use
The dist plot is a visual diagram using a confidence distribution.

## Required R Packages
- broom
- data.table
- ggdist
- ggplot2
- jsonlite
- modelr
- tidyr

## Minimal Reproducible Code
```r
# Load packages
library(broom)
library(data.table)
library(ggdist)
library(ggplot2)
library(jsonlite)
library(modelr)

# Prepare data
# Load data
data <- data.table::fread(jsonlite::read_json("https://hiplot.cn/ui/basic/ggdist/data.json")$exampleData$textarea[[1]])
data <- as.data.frame(data)

# Convert data structure
data[, 1] <- factor(data[, 1], levels = rev(unique(data[, 1])))
data <- tibble(data)
data2 = lm(response ~ condition, data = data)
data3 <- data_grid(data, condition) %>%
  augment(data2, newdata = ., se_fit = TRUE)

# View data
head(data)

# Create visualization
# Dist Plot
p <- ggplot(data3, aes_(y = as.name(colnames(data[1])))) +
  stat_dist_halfeye(aes(dist = "student_t", arg1 = df.residual(data2),
                        arg2 = .fitted, arg3 = .se.fit),
                    scale = .5) +
  geom_point(aes_(x = as.name(colnames(data[2]))),
             data = data, pch = "|", size = 2,
             position = position_nudge(y = -.15)) +
  ggtitle("ggdist Plot") + 
  xlab("response") + ylab("condition") +
  theme_ggdist() +
  theme(text = element_text(family = "Arial"),
        plot.title = element_text(size = 12,hjust = 0.5),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 10),
        axis.text.x = element_text(angle = 0, hjust = 0.5,vjust = 1),
        legend.position = "right",
        legend.direction = "vertical",
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 10))

p
```

## Key Parameters
- `position`: Position adjustment (identity, dodge, stack, fill)
- `stat`: Statistical transformation to use
- `theme`: Plot theme; tutorial uses `theme_ggdist()`

## Tips
- Adjust text size with `theme(text = element_text(size = 14))` for presentations
- See the full tutorial for additional customization options and advanced examples

## Full Tutorial
https://openbiox.github.io/Bizard/Hiplot/066-ggdist.html
