# Skill: Gene Ranking Dotplot (R)

## Category
Hiplot

## When to Use
Gene expression ranking visualization.

## Required R Packages
- RColorBrewer
- data.table
- ggplot2
- ggrepel
- jsonlite

## Minimal Reproducible Code
```r
# Load packages
library(RColorBrewer)
library(data.table)
library(ggplot2)
library(ggrepel)
library(jsonlite)

# Prepare data
# Load data
data <- data.table::fread(jsonlite::read_json("https://hiplot.cn/ui/basic/gene-rank/data.json")$exampleData$textarea[[1]])
data <- as.data.frame(data)

# Convert data structure
## ordered by log2FoldChange and pvalue
data <- data[order(-data$log2FC, data$pvalue), ]
## add the rank column
data$rank <- 1:nrow(data)
## get the top n up and down gene for labeling
top_n <- 5
top_n_up <- rownames(head(data, top_n))
top_n_down <- rownames(tail(data, top_n))
genes_to_label <- c(top_n_up, top_n_down)
data2 <- data[genes_to_label, ]

# View data
head(data)

# Create visualization
# Gene Ranking Dotplot
p <- 
  ggplot(data, aes(rank, log2FC, color = pvalue, size = abs(log2FC))) + 
  geom_point() + 
  scale_color_gradientn(colours = colorRampPalette(brewer.pal(11,'RdYlBu'))(100)) +
  geom_hline(yintercept = c(-1, 1), linetype = 2, size = 0.3) +
  geom_hline(yintercept = 0, linetype = 1, size = 0.5) +
  geom_vline(xintercept = median(data$rank), linetype = 2, size = 0.3) + 
  geom_text_repel(data = data2, aes(rank, log2FC, label = gene),
                  size = 3, color = "red") +
  xlab("") + ylab("") + 
  ylim(c(-max(abs(data$log2FC)), max(abs(data$log2FC)))) +
  labs(color = "Pvalue", size = "Log2FoldChange") +
  theme_bw(base_size = 12) +
  theme(text = element_text(family = "Arial"),
        plot.title = element_text(size = 12,hjust = 0.5),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 10),
        axis.text.x = element_text(angle = 0, hjust = 0.5,vjust = 1),
        legend.position = "right",
        legend.direction = "vertical",
        legend.title = element_text(size = 10),
# ... (see full tutorial for more)
```

## Key Parameters
- `color`: Maps `pvalue` to the color aesthetic
- `size`: Maps `abs` to the size aesthetic
- `position`: Position adjustment (identity, dodge, stack, fill)

## Tips
- Use `theme_minimal()` or `theme_bw()` for clean, publication-ready plots
- Customize color scales with `scale_fill_manual()` or `scale_color_brewer()`
- See the full tutorial for additional customization options and advanced examples

## Full Tutorial
https://openbiox.github.io/Bizard/Hiplot/061-gene-rank.html
