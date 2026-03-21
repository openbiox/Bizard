# Skill: Hi-C Heatmap (R)

## Category
Hiplot

## When to Use
The HiC heatmap is used to display the genome-wide chromatin interaction with heatmap on different chromosomes.

## Required R Packages
- RColorBrewer
- data.table
- ggplot2
- jsonlite

## Minimal Reproducible Code
```r
# Load packages
library(RColorBrewer)
library(data.table)
library(ggplot2)
library(jsonlite)

# Prepare data
# Load data
data <- data.table::fread(jsonlite::read_json("https://hiplot.cn/ui/basic/hic-heatmap/data.json")$exampleData$textarea[[1]])
data <- as.data.frame(data)

# View data
head(data)

# Create visualization
# Hi-C Heatmap
## Calculate the number of bins
bins_num <- max(data$index_bin1) + 1
## Set the resolution of HiC data
resolution <- 500
res <- resolution * 1000
# Set the separation unit to 50Mb
intervals <- 50
spacing <- intervals * 1000000
## Count the number of breaks
breaks_num <- (res * bins_num) / spacing
## Set breaks
breaks <- c()
for (i in 0:breaks_num) {
  breaks <- c(breaks, i * intervals)
}

p <- ggplot(data = data, aes(x = index_bin1 * res, y = index_bin2 * res)) +
  geom_tile(aes(fill = freq)) +
  scale_fill_gradientn(
    colours = colorRampPalette(rev(brewer.pal(11,"RdYlBu")))(500),
    limits = c(0, max(data$freq) * 1.2)
  ) +
  scale_y_reverse() +
  scale_x_continuous(breaks = breaks * 1000000, labels = paste0(breaks, "Mb")) +
  scale_y_continuous(breaks = breaks * 1000000, labels = paste0(breaks, "Mb")) +
  theme(panel.grid = element_blank(), axis.title = element_blank()) +
  labs(title = paste0("(resolution: ", res / 1000, "Kb)"), x="", y="") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "right", legend.key.size = unit(0.8, "cm"),
        panel.grid = element_blank())

p
```

## Key Parameters
- `x`: Maps `index_bin1` to the x aesthetic
- `y`: Maps `index_bin2` to the y aesthetic
- `fill`: Maps `freq` to the fill aesthetic
- `position`: Position adjustment (identity, dodge, stack, fill)
- `theme`: Plot theme; tutorial uses `theme_bw()`

## Tips
- Use `theme_minimal()` or `theme_bw()` for clean, publication-ready plots
- Customize color scales with `scale_fill_manual()` or `scale_color_brewer()`
- See the full tutorial for additional customization options and advanced examples

## Full Tutorial
https://openbiox.github.io/Bizard/Hiplot/087-hic-heatmap.html
