# Skill: Scatter Plot (Julia)

## Category
Julia

## When to Use
A scatter plot displays values for two continuous variables as a collection of points. Julia's `CairoMakie` package (part of the Makie.jl ecosystem) provides high-performance, GPU-accelerated plotting capabilities ideal for large biomedical datasets. Makie offers publication-quality rendering with a composable, declarative API.

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
n = 150
species = repeat(["setosa", "versicolor", "virginica"], inner=50)
sepal_length = [randn(50) .* 0.35 .+ 5.0;
                randn(50) .* 0.52 .+ 5.9;
                randn(50) .* 0.64 .+ 6.6]
sepal_width = [randn(50) .* 0.38 .+ 3.4;
               randn(50) .* 0.31 .+ 2.8;
               randn(50) .* 0.32 .+ 3.0]
iris_df = DataFrame(species=species, sepal_length=sepal_length, sepal_width=sepal_width)

# Create visualization
fig = Figure(size=(700, 500))
ax = Axis(fig[1,1], xlabel="Sepal Length (cm)", ylabel="Sepal Width (cm)",
          title="Iris Scatter Plot")
colors_map = Dict("setosa" => :steelblue, "versicolor" => :coral, "virginica" => :green)
for sp in unique(iris_df.species)
    mask = iris_df.species .== sp
    scatter!(ax, iris_df.sepal_length[mask], iris_df.sepal_width[mask],
             color=colors_map[sp], markersize=10, alpha=0.7, label=sp)
end
axislegend(ax, position=:rt)
fig
```

## Key Parameters
- `colormap`: Color scheme for the plot (e.g., :viridis, :RdBu)
- `markersize`: Size of scatter plot markers
- `color`: Color of plot elements (e.g., :steelblue or (:red, 0.5) for alpha)
- `linewidth`: Width of lines in the plot
- `alpha`: Transparency level (0–1) via color tuple (color, alpha)
- `width`: Width of violin or box plot elements
- `size`: Figure size as (width, height) in pixels

## Tips
- Save figures with `save("plot.png", fig)` or `save("plot.pdf", fig)`
- Adjust figure resolution with `Figure(size=(800, 600), figure_padding=20)`
- See the full tutorial for additional customization options and advanced examples

## Full Tutorial
https://openbiox.github.io/Bizard/Julia/ScatterPlot.html
