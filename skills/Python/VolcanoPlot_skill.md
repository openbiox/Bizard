# Skill: Volcano Plot (Python)

## Category
Python

## When to Use
A volcano plot displays statistical significance (-log10 p-value) versus fold-change (log2 FC) for thousands of features simultaneously. In biomedical research, volcano plots are the standard visualization for differential gene expression results from RNA-seq, proteomics, and metabolomics. Python's `matplotlib` provides full control over customizing these publication-ready plots.

## Required Python Packages
- matplotlib
- numpy
- pandas

## Minimal Reproducible Code
```python
# Load packages
import matplotlib.pyplot as plt
import pandas as pd
import numpy as np

# Prepare data
np.random.seed(42)
n_genes = 5000
df = pd.DataFrame({
    'gene': [f'Gene{i+1}' for i in range(n_genes)],
    'log2FC': np.random.normal(0, 1.5, n_genes),
    'pvalue': np.random.uniform(1e-10, 1, n_genes)
})
df['neg_log10p'] = -np.log10(df['pvalue'])

fc_thresh = 1.0
p_thresh = 0.05

conditions = [
    (df['log2FC'] > fc_thresh) & (df['pvalue'] < p_thresh),
    (df['log2FC'] < -fc_thresh) & (df['pvalue'] < p_thresh),
]
choices = ['Up', 'Down']
df['regulation'] = np.select(conditions, choices, default='NS')

# Create visualization
colors = {'Up': '#e63946', 'Down': '#457b9d', 'NS': '#cccccc'}
fig, ax = plt.subplots(figsize=(8, 6))
for reg, color in colors.items():
    subset = df[df['regulation'] == reg]
    ax.scatter(subset['log2FC'], subset['neg_log10p'],
               c=color, s=8, alpha=0.6, label=f'{reg} ({len(subset)})')
ax.axhline(-np.log10(p_thresh), color='grey', linestyle='--', linewidth=0.8)
ax.axvline(fc_thresh, color='grey', linestyle='--', linewidth=0.8)
ax.axvline(-fc_thresh, color='grey', linestyle='--', linewidth=0.8)
ax.set_xlabel('log₂(Fold Change)')
ax.set_ylabel('-log₁₀(P-value)')
ax.set_title('Volcano Plot')
ax.legend(frameon=False)
ax.spines[['top', 'right']].set_visible(False)
plt.tight_layout()
plt.show()
```

## Key Parameters
- `figsize`: Figure dimensions as (width, height) in inches
- `alpha`: Transparency level (0–1)
- `annot`: Whether to annotate cells with values (True/False)

## Tips
- The tutorial includes a 'Enhanced Volcano with Significance Regions' section with advanced styling options
- Call `plt.tight_layout()` to prevent label overlap
- See the full tutorial for additional customization options and advanced examples

## Full Tutorial
https://openbiox.github.io/Bizard/Python/VolcanoPlot.html
