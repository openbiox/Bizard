# Skill: Network Plot (R)

## Category
Omics

## When to use
In microbiome research, it is crucial to understand the interactions between microorganisms. Network analysis is a powerful method that can help us visualize and quantify these complex relationships. Next, we will introduce the network operation and annotation functions of the `MetaNet` package, which can make our network analysis more in-depth and intuitive.

## Required R packages
- MetaNet
- dplyr
- igraph
- pcutils

## Minimal reproducible code
```r
# Basic Network
data("multi_test", package = "MetaNet")
data("c_net", package = "MetaNet")
multi1 <- multi_net_build(list(Microbiome = micro, Metabolome = metab, Transcriptome = transc))
plot(multi1)
```

## Full tutorial
https://openbiox.github.io/Bizard/Omics/NetworkPlot.html
