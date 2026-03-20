# Skill: Scatter Plot (Julia) (Julia)

## Category
Julia

## When to use
A scatter plot displays values for two continuous variables as a collection of points. Julia's `CairoMakie` package (part of the Makie.jl ecosystem) provides high-performance, GPU-accelerated plotting capabilities ideal for large biomedical datasets. Makie offers publication-quality rendering with a composable, declarative API.

## Required Julia packages
- CairoMakie
- DataFrames

## Minimal reproducible code
```julia
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

## Full tutorial
https://openbiox.github.io/Bizard/Julia/ScatterPlot.html
