# Skill: Violin Plot (Python)

## Category
Python

## When to Use
A violin plot combines a box plot and a kernel density estimation to show the distribution of continuous data across categories. In biomedical research, violin plots are ideal for comparing gene expression distributions, drug response measurements, or clinical biomarker levels across patient groups. Python's `seaborn` library makes it simple to create beautiful violin plots.

## Required Python Packages
- matplotlib
- numpy
- pandas
- seaborn

## Minimal Reproducible Code
```python
# Load packages
import matplotlib.pyplot as plt
import seaborn as sns
import pandas as pd
import numpy as np

# Prepare data
iris = sns.load_dataset("iris")

np.random.seed(42)
n_per_group = 80
groups = ['Tumor', 'Normal', 'Adjacent']
gene_expr = pd.DataFrame({
    'Expression': np.concatenate([
        np.random.normal(8, 1.5, n_per_group),
        np.random.normal(5, 1.2, n_per_group),
        np.random.normal(6.5, 1.8, n_per_group)
    ]),
    'Group': np.repeat(groups, n_per_group),
    'Gene': np.tile(np.repeat(['TP53', 'BRCA1'], n_per_group // 2), 3)
})

# Create visualization
fig, ax = plt.subplots(figsize=(8, 6))
sns.violinplot(data=iris, x='species', y='sepal_length', palette='Set2',
               inner='box', ax=ax)
ax.set_xlabel('Species')
ax.set_ylabel('Sepal Length (cm)')
ax.set_title('Distribution of Sepal Length by Species')
ax.spines[['top', 'right']].set_visible(False)
plt.tight_layout()
plt.show()
```

## Key Parameters
- `palette`: Color palette for the plot (e.g., Set2, viridis, coolwarm)
- `figsize`: Figure dimensions as (width, height) in inches
- `alpha`: Transparency level (0–1)
- `inner`: Representation inside violin (box, quartile, point, stick, None)
- `hue`: Variable for color grouping

## Tips
- Call `plt.tight_layout()` to prevent label overlap
- Seaborn integrates with pandas DataFrames for convenient column-based plotting
- See the full tutorial for additional customization options and advanced examples

## Full Tutorial
https://openbiox.github.io/Bizard/Python/ViolinPlot.html
