# Skill: Diffusion Map (R)

## Category
Hiplot

## When to Use
Diffusion Map is a nonlinear dimensionality reduction algorithm that can be used to visualize developmental trajectories.

## Required R Packages
- BiocManager
- data.table
- destiny
- ggplotify
- ggpubr
- jsonlite
- scatterplot3d
- smoother

## Minimal Reproducible Code
```r
# Load packages
library(BiocManager)
library(data.table)
library(destiny)
library(ggplotify)
library(ggpubr)
library(jsonlite)

# Prepare data
# Load data
data1 <- data.table::fread(jsonlite::read_json("https://hiplot.cn/ui/basic/diffusion-map/data.json")$exampleData[[1]]$textarea[[1]])
data1 <- as.data.frame(data1)
data2 <- data.table::fread(jsonlite::read_json("https://hiplot.cn/ui/basic/diffusion-map/data.json")$exampleData[[1]]$textarea[[2]])
data2 <- as.data.frame(data2)

# convert data structure
sample.info <- data2
rownames(data1) <- data1[, 1]
data1 <- as.matrix(data1[, -1])
## tsne
set.seed(123)
dm_info <- DiffusionMap(t(data1))
dm_info <- cbind(DC1 = dm_info$DC1, DC2 = dm_info$DC2, DC3 = dm_info$DC3)
dm_data <- data.frame(
  sample = colnames(data1),
  dm_info
)

colorBy <- sample.info[match(colnames(data1), sample.info[, 1]), "Group"]
colorBy <- factor(colorBy, level = colorBy[!duplicated(colorBy)])
dm_data$colorBy = colorBy

# View data
head(dm_data)

# Create visualization
# 2D Diffusion Map
p <- ggscatter(data = dm_data,  x = "DC1", y = "DC2", color = "colorBy",
               size = 2, palette = "lancet", alpha = 1) +
  labs(color = "Group") +
  ggtitle("Diffusion Map") +
  scale_color_manual(values = c("#3B4992FF","#EE0000FF","#008B45FF")) +
  theme_classic() +
  theme(text = element_text(family = "Arial"),
        plot.title = element_text(size = 12,hjust = 0.5),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 10),
        axis.text.x = element_text(angle = 0, hjust = 0.5,vjust = 1),
        legend.position = "right",
        legend.direction = "vertical",
# ... (see full tutorial for more)
```

## Key Parameters
- `alpha`: Controls transparency (0 = fully transparent, 1 = opaque)
- `position`: Position adjustment (identity, dodge, stack, fill)
- `theme`: Plot theme; tutorial uses `theme_classic()`

## Tips
- Customize color scales with `scale_fill_manual()` or `scale_color_brewer()`
- Adjust text size with `theme(text = element_text(size = 14))` for presentations
- See the full tutorial for additional customization options and advanced examples

## Full Tutorial
https://openbiox.github.io/Bizard/Hiplot/042-diffusion-map.html
