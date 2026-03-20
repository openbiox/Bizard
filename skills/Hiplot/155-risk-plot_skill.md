# Skill: Risk Factor Analysis (R)

## Category
Hiplot

## When to use
::: callout-note
**Hiplot website**

## Required R packages
- cowplot
- cutoff
- data.table
- fastStat
- ggplot2
- jsonlite
- survminer

## Minimal reproducible code
```r
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
## Figure B
fB <- ggplot(data = data2, aes(x = 1:nrow(data2), y = data2[, "time"],
             color = factor(ifelse(data2[, "event"] == 1, "Dead", "Alive")))) +
  geom_point(size = 2) +
  scale_color_manual(name = "Status", values = c("Alive" = "#0B45A5", "Dead" = "#E20B0B")) +
  geom_vline(xintercept = cut.position, linetype = "dotted", size = 1) +
  theme(panel.grid = element_blank(), panel.background = element_blank(),
        axis.ticks.x = element_blank(), axis.line.x = element_blank(),
        axis.text.x = element_blank(), axis.title.x = element_blank(),
        axis.title.y = element_text(size = 14, vjust = 2, angle = 90),
        axis.text.y = element_text(size = 11),
        axis.ticks.y = element_line(size = 0.5),
        axis.line.y = element_line(size = 0.5, colour = "black"),
        legend.title = element_text(size = 13),
        legend.text = element_text(size = 12)) +
  ylab("Survival Time") +
  coord_trans() +
  scale_x_continuous(expand = c(0, 3))
## middle
middle <- ggplot(data2, aes(x = 1:nrow(data2), y = 1)) +
  geom_tile(aes(fill = data2$Group)) +
  scale_fill_manual(name = "Risk Group", values = c("Low" = "#0B45A5", "High" = "#E20B0B")) +
  theme(panel.grid = element_blank(), panel.background = element_blank(),
        axis.line = element_blank(), axis.ticks = element_blank(),
        axis.text = element_blank(), axis.title = element_blank(),
        plot.margin = unit(c(0.15, 0, -0.3, 0), "cm"),
        legend.title = element_text(size = 13),
        legend.text = element_text(size = 12)) +
  scale_x_continuous(expand = c(0, 3)) +
  xlab("")
## Figure C
heatmap_genes <- c("TAGLN2", "PDPN", "TIMP1", "EMP3")
data3 <- data[, heatmap_genes]
if (length(heatmap_genes) == 1) {
  data3 <- data.frame(data3)
  colnames(data3) <- heatmap_genes
}
# Normalization
for (i in 1:ncol(data3)) {
  data3[, i] <- (data3[, i] - mean(data3[, i], na.rm = TRUE)) /
  sd(data3[, i], na.rm = TRUE)
}
data4 <- cbind(id = 1:nrow(data3), data3)
data5 <- reshape2::melt(data4, id.vars = "id")
fC <- ggplot(data5, aes(x = id, y = variable, fill = value)) +
  geom_raster() +
  theme(panel.grid = element_blank(), panel.background = element_blank(),
        axis.line = element_blank(), axis.ticks = element_blank(),
        axis.text.x = element_blank(), axis.title = element_blank(),
        plot.background = element_blank()) +
  scale_fill_gradient2(name = "Expression", low = "#0B45A5", mid = "#FFFFFF",
                       high = "#E20B0B") +
  theme(axis.text = element_text(size = 11)) +
  theme(legend.title = element_text(size = 13), 
        legend.text = element_text(size = 12)) +
  scale_x_continuous(expand = c(0, 3))

p <- plot_grid(fA, fB, middle, fC, ncol = 1, rel_heights = c(0.1, 0.1, 0.01, 0.15))

p
```

## Full tutorial
https://openbiox.github.io/Bizard/Hiplot/155-risk-plot.html
