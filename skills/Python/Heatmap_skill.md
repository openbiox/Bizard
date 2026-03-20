# Skill: Heatmap (Python) (Python)

## Category
Python

## When to use
A heatmap is a data visualization technique that uses color to represent values in a matrix. In biomedical research, heatmaps are essential for visualizing gene expression profiles, correlation matrices, methylation data, and drug response panels. Python's `seaborn` and `matplotlib` libraries offer powerful heatmap capabilities with built-in clustering support.

## Required Python packages
- matplotlib
- numpy
- pandas
- scipy
- seaborn

## Minimal reproducible code
```python
fig, ax = plt.subplots(figsize=(10, 8))
sns.heatmap(expr_df, cmap='RdBu_r', center=0, xticklabels=True,
            yticklabels=True, linewidths=0.5, ax=ax)
ax.set_title('Gene Expression Heatmap')
ax.set_xlabel('Samples')
ax.set_ylabel('Genes')
plt.tight_layout()
plt.show()
```

## Full tutorial
https://openbiox.github.io/Bizard/Python/Heatmap.html
