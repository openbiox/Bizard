# Skill: Violin Plot (Julia)

## Category
Julia

## When to Use
A violin plot combines box plot statistics with kernel density estimation to show data distributions. Julia's `CairoMakie` makes it straightforward to create violin plots for comparing gene expression, biomarker levels, or clinical measurements across groups.

## Required Julia Packages
- CairoMakie
- DataFrames

## Minimal Reproducible Code
```julia
# Load packages
using CairoMakie
using DataFrames
using Random

# Prepare data
Random.seed!(42)
n_per = 80
groups = vcat(fill("Tumor", n_per), fill("Normal", n_per), fill("Adjacent", n_per))
expression = vcat(
    randn(n_per) .* 1.5 .+ 8,
    randn(n_per) .* 1.2 .+ 5,
    randn(n_per) .* 1.8 .+ 6.5
)
group_idx = vcat(fill(1, n_per), fill(2, n_per), fill(3, n_per))
df = DataFrame(Group=groups, Expression=expression, GroupIdx=group_idx)

# Create visualization
fig = Figure(size=(700, 500))
ax = Axis(fig[1,1], xlabel="Group", ylabel="Expression Level",
          title="Gene Expression Distribution",
          xticks=(1:3, ["Tumor", "Normal", "Adjacent"]))
violin!(ax, df.GroupIdx, df.Expression, color=(:steelblue, 0.7))
fig
```

## Key Parameters
- `markersize`: Size of scatter plot markers
- `color`: Color of plot elements (e.g., :steelblue or (:red, 0.5) for alpha)
- `side`: Side of violin to draw (:left, :right, or both)
- `width`: Width of violin or box plot elements
- `size`: Figure size as (width, height) in pixels
- `alpha`: Transparency via color tuple syntax: color=(:steelblue, 0.7)

## Tips
- Save figures with `save("plot.png", fig)` or `save("plot.pdf", fig)`
- Adjust figure resolution with `Figure(size=(800, 600), figure_padding=20)`
- See the full tutorial for additional customization options and advanced examples

## Full Tutorial
https://openbiox.github.io/Bizard/Julia/ViolinPlot.html
