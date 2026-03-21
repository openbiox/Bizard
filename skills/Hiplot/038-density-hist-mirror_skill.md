# Skill: Mirror Density & Histogram (R)

## Category
Hiplot

## When to Use
The mirror density & histogram is a graph used to observe the distribution of continuous variables in two side view: top and bottom.

## Required R Packages
- data.table
- ggplot2
- ggthemes
- jsonlite

## Minimal Reproducible Code
```r
# Load packages
library(data.table)
library(ggplot2)
library(ggthemes)
library(jsonlite)

# Prepare data
# Load data
data <- data.table::fread(jsonlite::read_json("https://hiplot.cn/ui/basic/density-hist-mirror/data.json")$exampleData$textarea[[1]])
data <- as.data.frame(data)

# convert data structure
sides <- data[1,]
data <- data[-1,]
for (i in 1:ncol(data)) {
  data[,i] <- as.numeric(data[,i])
}

# View data
head(data)

# Create visualization
# Mirror Density
p <- ggplot(data, aes(x=x))
colrs <- c("#e64b35ff","#4dbbd5ff","#00a087ff","#3c5488ff","#f39b7fff","#8491b4ff")
colrs2 <- colnames(data)
for (i in seq_len(length(sides))) {
  eval(parse(
    text = sprintf("p <- p + geom_density(aes(x = %s, y = %s..density.., color = '%s', fill = '%s'), kernel = '%s')", 
                   colnames(data)[i], ifelse(sides[i] == "top", "", "-"), colnames(data)[i],
                   colnames(data)[i], "gaussian")
    ))
  names(colrs)[i] <- colnames(data)[i]
  names(colrs2)[i] <- colrs[i]
}
p <- p + 
  ggtitle("") +
  scale_fill_manual(values=colrs, name="Densities") +
  scale_color_manual(values=colrs, name="Densities") +
  theme_stata() +
  theme(text = element_text(family = "Arial"),
        plot.title = element_text(size = 12,hjust = 0.5),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 10),
        axis.text.x = element_text(angle = 0, hjust = 0.5,vjust = 1),
        legend.position = "right",
        legend.direction = "vertical",
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 10))

# ... (see full tutorial for more)
```

## Key Parameters
- `x`: Maps `x` to the x aesthetic
- `position`: Position adjustment (identity, dodge, stack, fill)
- `stat`: Statistical transformation to use
- `theme`: Plot theme; tutorial uses `theme_stata()`

## Tips
- Customize color scales with `scale_fill_manual()` or `scale_color_brewer()`
- Adjust text size with `theme(text = element_text(size = 14))` for presentations
- See the full tutorial for additional customization options and advanced examples

## Full Tutorial
https://openbiox.github.io/Bizard/Hiplot/038-density-hist-mirror.html
