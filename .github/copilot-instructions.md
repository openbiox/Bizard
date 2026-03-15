# Bizard – GitHub Copilot Code Review Instructions

## Project Overview
Bizard is a **Biomedical Visualization Atlas** — a community-driven Quarto website that
hosts bilingual (English + Chinese) R-based visualization tutorials for biomedical
researchers. Every tutorial is a `.qmd` (Quarto Markdown) file rendered by Quarto and
published to GitHub Pages.

---

## Tutorial QMD Format Requirements

When reviewing a new or modified `.qmd` tutorial, check for the following.

### 1. YAML Frontmatter (required)
Every tutorial must start with a YAML block containing:
```yaml
---
title: "Plot Name"
author:
  - "**[Editor]** [Name](https://github.com/username);"
  - "**[Contributors]** [Name](https://github.com/username)."
---
```
- `title` must be present and descriptive.
- `author` must list at least one Editor and optionally Contributors.

### 2. Required Sections (in order)
Each tutorial should contain, in this order:
1. **A description paragraph** (immediately after the frontmatter, before any heading).
2. `## Example` – contains a demo image `![](../images/<Category>/<Name>_demo.png){...}`.
3. `## Setup` – lists: System Requirements, Programming Language, Dependencies, and an `{r packages setup}` code block.
4. `## Data Preparation` – loads data, preferably from public biomedical sources.
5. `## Visualization` – contains one or more `{r fig...}` code blocks with `fig-cap` and `label`.

### 3. Code Block Conventions
- The packages setup block must be named: `` ```{r packages setup, message=FALSE, warning=FALSE, output=FALSE} ``
- Figure blocks must have `label`, `fig-cap`, and `out.width` options.
- Include a `sessioninfo::session_info("attached")` block after setup.
- All packages must check for installation before loading (`if (!requireNamespace(...)) install.packages(...)`).
- Code must be self-contained and reproducible using public datasets.

### 4. Bilingual Pairing
- Every English `.qmd` should have a corresponding `.zh.qmd` Chinese translation.
- The auto-translate workflow generates `.zh.qmd` automatically; do not manually edit `.zh.qmd` files unless correcting mistranslations.

### 5. Images
- Demo images go in `images/<Category>/` with the naming convention `<TutorialName>_demo.png`.
- Reference them as `![](../images/<Category>/<TutorialName>_demo.png){fig-alt="..." fig-align="center" width="60%"}`.

### 6. Data Sources
- Prefer public biomedical datasets: TCGA, GEO (GSExxx), built-in R datasets.
- Large data files must be hosted on Bizard Tencent COS and referenced by URL, not committed to the repo.
- Dataset size should be < 1 MB.

### 7. Style
- Use `-` for unordered lists, not `*`.
- Use backtick inline code for package names, function names, and file paths.
- Avoid line lengths > 120 characters in prose.
- Keep the tutorial self-contained: a reader should be able to copy-paste the code and reproduce every figure.

---

## Workflow Files
When reviewing `.github/workflows/*.yml` files:
- Prefer reusable `actions/github-script@v7` for GitHub API calls.
- Always pin third-party actions to a major version tag (e.g., `@v4`, `@v5`).
- Avoid storing secrets in `run:` steps; use `env:` to pass secrets to steps.
- Translation workflow should not overwrite existing `.zh.qmd` files unless the user explicitly requests it.

---

## What to Flag
- Missing `title` or `author` in YAML.
- Absence of `## Example`, `## Setup`, `## Data Preparation`, or `## Visualization`.
- Hardcoded local file paths (e.g., `read.csv("/home/user/data.csv")`).
- Direct commit of large binary files or data files > 1 MB.
- Code blocks without proper `label` for figures.
- Missing `sessioninfo::session_info("attached")` block.
- Missing bilingual pair (`.zh.qmd`) when adding a new tutorial.
- Packages loaded without the standard `if (!requireNamespace(...))` guard.
