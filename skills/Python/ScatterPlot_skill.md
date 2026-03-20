# Skill: Scatter Plot (Python) (Python)

## Category
Python

## When to use
A scatter plot displays values for two continuous variables as a collection of points. In biomedical research, scatter plots are widely used for visualizing correlations between gene expression levels, comparing biomarkers, and exploring relationships in multi-omics datasets. Python's `matplotlib` and `seaborn` libraries provide flexible and publication-quality scatter plot capabilities.

## Required Python packages
- matplotlib
- numpy
- pandas
- scipy
- seaborn

## Minimal reproducible code
```python
fig, ax = plt.subplots(figsize=(8, 6))
for species in iris['species'].unique():
    subset = iris[iris['species'] == species]
    ax.scatter(subset['sepal_length'], subset['sepal_width'],
               label=species, alpha=0.7, edgecolors='white', linewidth=0.5)
ax.set_xlabel('Sepal Length (cm)')
ax.set_ylabel('Sepal Width (cm)')
ax.set_title('Iris Scatter Plot')
ax.legend(title='Species')
ax.spines[['top', 'right']].set_visible(False)
plt.tight_layout()
plt.show()
```

## Full tutorial
https://openbiox.github.io/Bizard/Python/ScatterPlot.html
