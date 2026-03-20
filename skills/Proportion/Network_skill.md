# Skill: Network Graph (R)

## Category
Proportion

## When to use
A network graph is a graphical model that resembles a network and consists of nodes and links, where links can be directed or undirected.

## Required R packages
- RColorBrewer
- cowplot
- igraph
- networkD3

## Minimal reproducible code
```r
# plot----
par(mfrow=c(2,2), mar=c(1,1,1,1))
plot(network1, main="Adjacency matrix (square matrix)")
plot(network2, main="Incident matrix")
plot(network3, main="Edge List")
plot(network4, main="Linked text list")
```

## Full tutorial
https://openbiox.github.io/Bizard/Proportion/Network.html
