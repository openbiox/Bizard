# Skill: Network Graph (R)

## Category
Proportion

## When to Use
A network graph is a graphical model that resembles a network and consists of nodes and links, where links can be directed or undirected.

## Required R Packages
- RColorBrewer
- cowplot
- igraph
- networkD3

## Minimal Reproducible Code
```r
# Load packages
library(RColorBrewer)
library(cowplot)
library(igraph)
library(networkD3)

# Prepare data
# network_pmat
## Correlation analysis results of various indicators in the Matcars dataset
data_pmat_links<- readr::read_csv("https://bizard-1301043367.cos.ap-guangzhou.myqcloud.com/data_mat.csv")
data_pmat_links <- na.omit(data_pmat_links)
colnames(data_pmat_links) <- c("from", "to", "p.log")

data_pmat_node <- colnames(mtcars)
network_pmat <- graph_from_data_frame(d=data_pmat_links, vertices=data_pmat_node, directed=F) 

# data_mat
## mtcars clustering analysis
## Calculate the correlation coefficient matrix
data_mat <- cor(t(mtcars[,c(1,3:6)]))
## Filtering highly relevant data
data_mat[data_mat<0.995] <- 0

# Create visualization
# plot----
par(mfrow=c(2,2), mar=c(1,1,1,1))
plot(network1, main="Adjacency matrix (square matrix)")
plot(network2, main="Incident matrix")
plot(network3, main="Edge List")
plot(network4, main="Linked text list")
```

## Key Parameters
- `width`: Controls element width
- `fill`: Maps a variable to fill color for group comparison
- `color`: Maps a variable to outline/point color

## Tips
- Adjust text size with `theme(text = element_text(size = 14))` for presentations
- See the full tutorial for additional customization options and advanced examples

## Full Tutorial
https://openbiox.github.io/Bizard/Proportion/Network.html
