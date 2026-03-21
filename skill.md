# Bizard Skill Generation Guide

> **Purpose**: This document provides a professional specification for LLM models to
> convert Bizard tutorial QMD documents into high-quality AI skill files. Each skill
> file serves as a concise, actionable reference that enables LLMs and researchers to
> quickly reproduce biomedical visualizations.

---

## What Is a Skill?

A **skill** is a structured Markdown document that distills a Bizard tutorial into an
LLM-friendly reference card. It captures:

- **What** the visualization does and when to use it
- **How** to produce it (language, packages, data, code)
- **Where** to find the full tutorial for deeper learning

Skills are consumed by AI assistants, copilots, and researchers who need a quick,
self-contained recipe for a specific visualization technique.

---

## Skill File Specification

### File Naming

```
skills/<Category>/<TutorialName>_skill.md
```

- `<Category>` matches the tutorial directory (e.g., `Distribution`, `Omics`, `Hiplot`)
- `<TutorialName>` matches the QMD filename without extension (e.g., `ViolinPlot`)
- Always use `_skill.md` suffix

### Document Structure

Every skill file **must** contain exactly these sections in order:

```markdown
# Skill: <Title> (<Language>)

## Category
<Category name>

## When to Use
<1–3 sentence description of what this visualization shows and when a researcher should choose it.>

## Required <Language> Packages
- package1
- package2
- ...

## Minimal Reproducible Code

```<language>
<Complete, self-contained code that produces the visualization.>
```

## Key Parameters
- `param1`: <brief description of what it controls>
- `param2`: <brief description>

## Tips
- <Practical tip 1 for better results>
- <Practical tip 2>

## Full Tutorial
<URL to the rendered tutorial page>
```

---

## Section-by-Section Guidelines

### 1. Title Line

```markdown
# Skill: <Title> (<Language>)
```

- Use the exact tutorial title from the YAML `title:` field
- Append the primary language in parentheses: `(R)`, `(Python)`, or `(Julia)`
- Example: `# Skill: Volcano Plot (R)`

### 2. Category

```markdown
## Category
Distribution
```

Use one of the official Bizard categories:
`Distribution`, `Correlation`, `Ranking`, `Composition`, `Proportion`,
`DataOverTime`, `Animation`, `Omics`, `Clinics`, `Hiplot`, `Python`, `Julia`

For tutorials in the `Python/` or `Julia/` directories, set the category to the
visualization type if identifiable (e.g., `Distribution` for a Python Violin Plot),
or fall back to `Python` / `Julia`.

### 3. When to Use

```markdown
## When to Use
<1–3 sentences>
```

Write a clear, practical description that answers:
- What does this visualization display?
- What type of data does it work with?
- In what research context should one choose this chart?

**Good**: "A volcano plot displays statistical significance (−log₁₀ p-value) against
fold change (log₂ FC) for differential expression analysis. Use it to identify
significantly up- or down-regulated genes after a two-group comparison."

**Bad**: "This is a plot." / "Volcano plot for data."

### 4. Required Packages

```markdown
## Required <Language> Packages
- ggplot2
- dplyr
```

- List **only** the packages explicitly loaded in the tutorial
- Exclude base/standard library modules:
  - **R**: `base`, `stats`, `utils`, `grDevices`, `graphics`, `methods`, `datasets`
  - **Python**: `os`, `sys`, `re`, `math`, `json`, `csv`, `collections`, `itertools`,
    `functools`, `pathlib`, `typing`, `datetime`, `random`, `copy`, `io`, `string`,
    `warnings`, `abc`, `enum`, `contextlib`, `textwrap`, `operator`, `subprocess`,
    `tempfile`, `shutil`, `glob`, `time`, `hashlib`, `urllib`
  - **Julia**: `Base`, `Core`, `Main`, `Printf`, `Random`, `Statistics`, `LinearAlgebra`,
    `Dates`, `Test`, `Pkg`
- Sort alphabetically
- Use the package name as imported (e.g., `scikit-learn` → list as `sklearn` if
  that is how it is imported, or note the pip name)

### 5. Minimal Reproducible Code

````markdown
## Minimal Reproducible Code

```r
# Load packages
library(ggplot2)

# Prepare sample data
data <- data.frame(
  group = rep(c("A", "B"), each = 50),
  value = c(rnorm(50, 5), rnorm(50, 7))
)

# Create visualization
ggplot(data, aes(x = group, y = value, fill = group)) +
  geom_violin() +
  theme_minimal()
```
````

Requirements:
- **Self-contained**: The code must run from a clean R/Python/Julia session with only
  the listed packages installed
- **Uses sample data**: Prefer built-in datasets (e.g., `iris`, `mtcars`,
  `palmerpenguins::penguins`) or generate synthetic data inline. Never reference
  external files unless absolutely necessary
- **Produces output**: The code must produce a visible plot when executed
- **Includes comments**: Brief comments explaining each logical step
- **Represents the tutorial**: Choose the most representative or visually impressive
  example from the tutorial, not necessarily the simplest
- **Length**: Aim for 10–40 lines; include enough to be useful but not overwhelming

### 6. Key Parameters

```markdown
## Key Parameters
- `fill`: Maps a variable to violin fill colors for group comparison
- `trim`: If `TRUE`, trims violin tails to the data range
- `scale`: Controls violin width; `"area"` (default), `"count"`, or `"width"`
```

- List 3–8 of the most important parameters that control the visualization
- Focus on parameters that researchers commonly adjust
- Include default values when helpful

### 7. Tips

```markdown
## Tips
- Combine with `geom_boxplot(width = 0.1)` inside the violin for summary statistics
- Use `coord_flip()` for horizontal violins when group labels are long
- Set `alpha < 1` for transparency when overlaying multiple layers
```

- Include 2–5 practical tips
- Focus on customization, best practices, and common pitfalls
- Draw from the tutorial's advanced sections and beautification steps

### 8. Full Tutorial Link

```markdown
## Full Tutorial
https://openbiox.github.io/Bizard/<Category>/<TutorialName>.html
```

Use the canonical URL on the Bizard GitHub Pages site.

---

## Conversion Process (for LLMs)

When converting a QMD tutorial to a skill file, follow these steps:

1. **Read the YAML frontmatter** → extract `title`
2. **Identify the language** → scan for `{r}`, `{python}`, or `{julia}` code blocks
3. **Determine the category** → use the parent directory name
4. **Extract the description** → take the first paragraph after the YAML frontmatter
   (before any `##` heading)
5. **Collect packages** → scan all code blocks for `library()`, `require()`,
   `import`, `from ... import`, `using` statements
6. **Select the best code example** → prefer figure-producing blocks (`fig-` label),
   ideally one that is visually appealing and self-contained. If the tutorial has
   multiple examples, pick the one that best showcases the visualization type
7. **Identify key parameters** → look at `aes()`, `geom_*()`, and theme function
   arguments that are commonly customized
8. **Extract tips** → look at beautification steps, advanced sections, and notes
   in the tutorial
9. **Compose the skill** → assemble all sections following the specification above
10. **Validate** → ensure the code block is self-contained and the package list is
    complete

---

## Quality Checklist

Before finalizing a skill file, verify:

- [ ] Title matches tutorial and includes language tag
- [ ] Category is one of the official categories
- [ ] "When to Use" is clear, specific, and 1–3 sentences
- [ ] Package list is complete and sorted alphabetically
- [ ] Code is self-contained and produces a visible plot
- [ ] Code includes brief comments
- [ ] Key Parameters section has 3–8 relevant parameters
- [ ] Tips section has 2–5 practical tips
- [ ] Tutorial URL is correct and uses the canonical format
- [ ] No trailing whitespace or formatting inconsistencies

---

## JSON Index Format

In addition to individual skill files, maintain two JSON files:

### `skills/index.json` (lightweight)

```json
[
  {
    "name": "Violin Plot",
    "category": "Distribution",
    "language": "R",
    "packages": ["ggplot2", "dplyr", "viridis"],
    "use_when": "Visualize data distribution shape across groups...",
    "tutorial_url": "https://openbiox.github.io/Bizard/Distribution/ViolinPlot.html",
    "source_file": "Distribution/ViolinPlot.qmd",
    "skill_file": "skills/Distribution/ViolinPlot_skill.md"
  }
]
```

### `skills/bizard_skills.json` (full)

Same structure as `index.json` but with an additional `"content"` field containing
the full Markdown skill document.
