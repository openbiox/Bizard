# Skill: Violin Plot (Python) (Python)

## Category
Python

## When to use
A violin plot combines a box plot and a kernel density estimation to show the distribution of continuous data across categories. In biomedical research, violin plots are ideal for comparing gene expression distributions, drug response measurements, or clinical biomarker levels across patient groups. Python's `seaborn` library makes it simple to create beautiful violin plots.

## Required Python packages
- matplotlib
- numpy
- pandas
- seaborn

## Minimal reproducible code
```python
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

## Full tutorial
https://openbiox.github.io/Bizard/Python/ViolinPlot.html
