# Skill: Network Plot (R)

## Category
Omics

## When to Use
In microbiome research, it is crucial to understand the interactions between microorganisms. Network analysis is a powerful method that can help us visualize and quantify these complex relationships. Next, we will introduce the network operation and annotation functions of the `MetaNet` package, which can make our network analysis more in-depth and intuitive.

## Required R Packages
- MetaNet
- dplyr
- igraph
- pcutils

## Minimal Reproducible Code
```r
# Load packages
library(MetaNet)
library(dplyr)
library(igraph)
library(pcutils)

# Prepare data
data(otutab, package = "pcutils")
t(otutab) -> totu
c_net_calculate(totu, method = "spearman") -> corr
c_net_build(corr, r_threshold = 0.6, p_threshold = 0.05, delete_single = T) -> co_net
class(co_net)

# Create visualization
# Basic Network
data("multi_test", package = "MetaNet")
data("c_net", package = "MetaNet")
multi1 <- multi_net_build(list(Microbiome = micro, Metabolome = metab, Transcriptome = transc))
plot(multi1)
```

## Key Parameters
- `fill`: Maps a variable to fill color for group comparison
- `color`: Maps a variable to outline/point color

## Tips
- Adjust text size with `theme(text = element_text(size = 14))` for presentations
- Include appropriate statistical thresholds (e.g., FDR < 0.05, |log2FC| > 1) in the visualization
- See the full tutorial for additional customization options and advanced examples

## Full Tutorial
https://openbiox.github.io/Bizard/Omics/NetworkPlot.html
