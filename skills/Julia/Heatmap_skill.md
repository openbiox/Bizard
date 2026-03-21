# Skill: Heatmap (Julia) (Julia)

## Category
Julia

## When to Use
A heatmap visualizes matrix data using color gradients. Julia's `CairoMakie` provides high-performance heatmap rendering suitable for large gene expression matrices and multi-omics data. The Makie ecosystem supports annotations, clustering, and complex layouts for publication-quality figures.

## Required Julia Packages
- CairoMakie

## Minimal Reproducible Code
```julia
# Load packages
using CairoMakie

# Prepare data
Random.seed!(42)
n_genes = 20
n_samples = 10
expr_matrix = randn(n_genes, n_samples)
expr_matrix[1:8, 1:5] .+= 2.5
expr_matrix[9:15, 6:10] .+= 2.0
gene_names = ["Gene_$i" for i in 1:n_genes]
sample_names = ["S$i" for i in 1:n_samples]

# Create visualization
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

## Key Parameters
- `colormap`: Color scheme for the plot (e.g., :viridis, :RdBu)
- `color`: Color of plot elements
- `colorrange`: Range for color mapping as (min, max) tuple

## Tips
- Save figures with `save("plot.png", fig)` or `save("plot.pdf", fig)`
- Adjust figure resolution with `Figure(size=(800, 600), figure_padding=20)`
- See the full tutorial for additional customization options and advanced examples

## Full Tutorial
https://openbiox.github.io/Bizard/Julia/Heatmap.html
