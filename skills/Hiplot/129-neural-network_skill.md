# Skill: Neural Network (R)

## Category
Hiplot

## When to Use
::: callout-note
**Hiplot website**

## Required R Packages
- NeuralNetTools
- data.table
- jsonlite
- nnet

## Minimal Reproducible Code
```r
# Neural Network
mod <- nnet(Y1 ~ X1 + X2 + X3, data = neuraldat, size = 10,
            maxint = 100, decay = 0)

# plot
par(mar = numeric(4))
plotnet(mod)
```

## Full Tutorial
https://openbiox.github.io/Bizard/Hiplot/129-neural-network.html
