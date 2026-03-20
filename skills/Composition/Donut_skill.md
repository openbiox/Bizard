# Skill: Donut Chart (R)

## Category
Composition

## When to use
A donut chart is a circular plot divided into sectors, each sector representing a part of the whole. It is very similar to a pie chart and can be constructed in ggplot2 and basic R.

## Required R packages
- ggplot2

## Minimal reproducible code
```r
# Data Preparation
counts <- table(TCGA_cli_df$T)
counts <- as.data.frame(counts)
names(counts)[names(counts) == "Var1"] <- "T"
# Calculate percentage
counts$fraction = counts$Freq / sum(counts$Freq)
# Calculate the cumulative percentage (the value at the top of each rectangle).
counts$ymax = cumsum(counts$fraction)
# Calculate the bottom of each rectangle to determine the starting position
counts$ymin = c(0, head(counts$ymax, n=-1))
# Plot
p <- ggplot(counts, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=T)) +
  geom_rect() +
  coord_polar(theta="y") + 
  xlim(c(2, 4)) 

p
```

## Full tutorial
https://openbiox.github.io/Bizard/Composition/Donut.html
