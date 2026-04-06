# Skill: Scatter Plot (Python)

## Category
Python

## When to Use
A scatter plot displays values for two continuous variables as a collection of points. In biomedical research, scatter plots are widely used for visualizing correlations between gene expression levels, comparing biomarkers, and exploring relationships in multi-omics datasets. Python's `matplotlib` and `seaborn` libraries provide flexible and publication-quality scatter plot capabilities.

## Required Python Packages
- matplotlib
- numpy
- pandas
- scipy
- seaborn

## Minimal Reproducible Code
```python
# Load packages
import matplotlib.pyplot as plt
import seaborn as sns
import pandas as pd
import numpy as np
from scipy import stats

# Prepare data
iris = sns.load_dataset("iris")

np.random.seed(42)
n = 200
gene_data = pd.DataFrame({
    'GeneA': np.random.normal(5, 2, n),
    'GeneB': np.random.normal(5, 2, n),
    'Group': np.random.choice(['Tumor', 'Normal'], n)
})
gene_data.loc[gene_data['Group'] == 'Tumor', 'GeneA'] += 2
gene_data.loc[gene_data['Group'] == 'Tumor', 'GeneB'] += 1.5

# Create visualization
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

## Key Parameters
- `palette`: Color palette for the plot (e.g., Set2, viridis, coolwarm)
- `figsize`: Figure dimensions as (width, height) in inches
- `alpha`: Transparency level (0–1)
- `hue`: Variable for color grouping

## Tips
- Call `plt.tight_layout()` to prevent label overlap
- Seaborn integrates with pandas DataFrames for convenient column-based plotting
- See the full tutorial for additional customization options and advanced examples

## Full Tutorial
https://openbiox.github.io/Bizard/Python/ScatterPlot.html
