# Skill: Line Regression (R)

## Category
Hiplot

## When to use
::: callout-note
**Hiplot website**

## Required R packages
- data.table
- ggplot2
- ggrepel
- jsonlite

## Minimal reproducible code
```r
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
        legend.position = "right",
        legend.direction = "vertical",
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 10))
  
## Add annotations for each group using ggrepel
repels <- rep("", nrow(data))
for (g in unique(data$group)) {
  fit <- lm(value2 ~ value1, data = data[data$group == g, ])
  v <- max(data[data$group == g, "value2"])
  repels[which(data$value2 == v)[1]] <- equation(fit, add_p = F)
}
p <- p + geom_text_repel(
  data = data,
  label = repels,
  size = 4,
  force = 5,
  label.padding = 5,
  na.rm = TRUE,
  min.segment.length = 100,
  show.legend = FALSE,
  nudge_x = 0,
  nudge_y = 0
  )

p
```

## Full tutorial
https://openbiox.github.io/Bizard/Hiplot/094-line-regression.html
