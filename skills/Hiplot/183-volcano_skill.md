# Skill: Volcano (R)

## Category
Hiplot

## When to Use
The volcanogram is a visual representation of the difference in gene expression between two samples.

## Required R Packages
- data.table
- ggpubr
- jsonlite

## Minimal Reproducible Code
```r
# Load packages
library(data.table)
library(ggpubr)
library(jsonlite)

# Prepare data
# Load data
data <- data.table::fread(jsonlite::read_json("https://hiplot.cn/ui/basic/volcano/data.json")$exampleData$textarea[[1]])
data <- as.data.frame(data)

# convert data structure
## Perform log10 transformation on the difference p (adj.P.Val column)
data[, "logP"] <- -log10(as.numeric(data[, "P.Value"]))
data[, "logFC"] <- as.numeric(data[, "logFC"])
## Add a new column Group
data[, "Group"] <- "not-significant"
## Up and down
data$Group[which((data[, "P.Value"] < 0.05) & (data$logFC >= 2))] <- "Up-regulated"
data$Group[which((data[, "P.Value"] < 0.05) & (data$logFC <= 2 * -1))] <- "Down-regulated"
## Add a new column Label
data[["Label"]] <- ""
## Sort the p-values of differentially expressed genes from small to large
data <- data[order(data[, "P.Value"]), ]
## Among the highly expressed genes, select the 10 with the smallest adj.P.Val
up_genes <- head(data[, "Symbol"][which(data$Group == "Up-regulated")], 10)
down_genes <- head(data[, "Symbol"][which(data$Group == "Down-regulated")], 10)
not_sig_genes <- NA
## Merge up_genes and down_genes and add them to Label
deg_top_genes <- c(as.character(up_genes), as.character(not_sig_genes),
as.character(down_genes))
deg_top_genes <- deg_top_genes[!is.na(deg_top_genes)]
data$Label[match(deg_top_genes, data[, "Symbol"])] <- deg_top_genes

# View data
head(data)

# Create visualization
# Volcano
options(ggrepel.max.overlaps = 100)
p <- ggscatter(data, x = "logFC", y = "logP", color = "Group", 
               palette = c("#2f5688", "#BBBBBB", "#CC0000"), size = 1, 
               alpha = 0.5, font.label = 8, repel = TRUE, label=data$Label,
               xlab = "log2(Fold Change)", ylab = "-log10(P Value)",
               show.legend.text = FALSE) +
  ggtitle("Volcano Plot") +
  geom_hline(yintercept = -log(0.05, 10), linetype = "dashed") +
  geom_vline(xintercept = c(2, -2), linetype = "dashed") +
  theme_bw() +
  theme(text = element_text(family = "Arial"),
        plot.title = element_text(size = 12,hjust = 0.5),
# ... (see full tutorial for more)
```

## Key Parameters
- `alpha`: Controls transparency (0 = fully transparent, 1 = opaque)
- `position`: Position adjustment (identity, dodge, stack, fill)
- `theme`: Plot theme; tutorial uses `theme_bw()`

## Tips
- Use `theme_minimal()` or `theme_bw()` for clean, publication-ready plots
- Adjust text size with `theme(text = element_text(size = 14))` for presentations
- See the full tutorial for additional customization options and advanced examples

## Full Tutorial
https://openbiox.github.io/Bizard/Hiplot/183-volcano.html
