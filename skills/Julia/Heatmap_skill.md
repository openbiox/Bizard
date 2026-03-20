# Skill: Heatmap (Julia) (Julia)

## Category
Julia

## When to use
A heatmap visualizes matrix data using color gradients. Julia's `CairoMakie` provides high-performance heatmap rendering suitable for large gene expression matrices and multi-omics data. The Makie ecosystem supports annotations, clustering, and complex layouts for publication-quality figures.

## Required Julia packages
- CairoMakie

## Minimal reproducible code
```julia
fig = Figure(size=(700, 600))
ax = Axis(fig[1,1], xlabel="Samples", ylabel="Genes",
          title="Gene Expression Heatmap",
          xticks=(1:n_samples, sample_names),
          yticks=(1:n_genes, gene_names))
hm = heatmap!(ax, 1:n_samples, 1:n_genes, expr_matrix',
              colormap=:RdBu, colorrange=(-3, 3))
Colorbar(fig[1,2], hm, label="Expression (z-score)")
fig
```

## Full tutorial
https://openbiox.github.io/Bizard/Julia/Heatmap.html
