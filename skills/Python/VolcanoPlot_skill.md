# Skill: Volcano Plot (Python) (Python)

## Category
Python

## When to use
A volcano plot displays statistical significance (-log10 p-value) versus fold-change (log2 FC) for thousands of features simultaneously. In biomedical research, volcano plots are the standard visualization for differential gene expression results from RNA-seq, proteomics, and metabolomics. Python's `matplotlib` provides full control over customizing these publication-ready plots.

## Required Python packages
- matplotlib
- numpy
- pandas

## Minimal reproducible code
```python
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

## Full tutorial
https://openbiox.github.io/Bizard/Python/VolcanoPlot.html
