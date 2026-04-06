# Skill: Heatmap (Python)

## Category
Python

## When to Use
A heatmap is a data visualization technique that uses color to represent values in a matrix. In biomedical research, heatmaps are essential for visualizing gene expression profiles, correlation matrices, methylation data, and drug response panels. Python's `seaborn` and `matplotlib` libraries offer powerful heatmap capabilities with built-in clustering support.

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
from scipy.cluster.hierarchy import linkage

# Prepare data
np.random.seed(42)
n_genes = 30
n_samples = 12
gene_names = [f'Gene_{i+1}' for i in range(n_genes)]
sample_names = [f'Sample_{i+1}' for i in range(n_samples)]
groups = ['Tumor'] * 6 + ['Normal'] * 6

expr_matrix = np.random.randn(n_genes, n_samples)
expr_matrix[:10, :6] += 2.5
expr_matrix[10:20, 6:] += 2.0

expr_df = pd.DataFrame(expr_matrix, index=gene_names, columns=sample_names)

# Create visualization
fig, ax = plt.subplots(figsize=(10, 8))
sns.heatmap(expr_df, cmap='RdBu_r', center=0, xticklabels=True,
            yticklabels=True, linewidths=0.5, ax=ax)
ax.set_title('Gene Expression Heatmap')
ax.set_xlabel('Samples')
ax.set_ylabel('Genes')
plt.tight_layout()
plt.show()
```

## Key Parameters
- `figsize`: Figure dimensions as (width, height) in inches
- `cmap`: Colormap for continuous color mapping
- `annot`: Whether to annotate cells with values (True/False)

## Tips
- Call `plt.tight_layout()` to prevent label overlap
- Seaborn integrates with pandas DataFrames for convenient column-based plotting
- See the full tutorial for additional customization options and advanced examples

## Full Tutorial
https://openbiox.github.io/Bizard/Python/Heatmap.html
