# Skill: Violin Plot (Julia) (Julia)

## Category
Julia

## When to Use
A violin plot combines box plot statistics with kernel density estimation to show data distributions. Julia's `CairoMakie` makes it straightforward to create violin plots for comparing gene expression, biomarker levels, or clinical measurements across groups.

## Required Julia Packages
- CairoMakie
- DataFrames

## Minimal Reproducible Code
```julia
fig = Figure(size=(700, 500))
ax = Axis(fig[1,1], xlabel="Group", ylabel="Expression Level",
          title="Gene Expression Distribution",
          xticks=(1:3, ["Tumor", "Normal", "Adjacent"]))
violin!(ax, df.GroupIdx, df.Expression, color=:steelblue, alpha=0.7)
fig
```

## Full Tutorial
https://openbiox.github.io/Bizard/Julia/ViolinPlot.html
