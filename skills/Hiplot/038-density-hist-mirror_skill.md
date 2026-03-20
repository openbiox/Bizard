# Skill: Mirror Density & Histogram (R)

## Category
Hiplot

## When to use
::: callout-note
**Hiplot website**

## Required R packages
- data.table
- ggplot2
- ggthemes
- jsonlite

## Minimal reproducible code
```r
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

p
```

## Full tutorial
https://openbiox.github.io/Bizard/Hiplot/038-density-hist-mirror.html
