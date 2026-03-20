# Bizard – GitHub Copilot Instructions

## Project Overview

Bizard is a **Biomedical Visualization Atlas** — a community-driven Quarto website that
hosts bilingual (English + Chinese) R-based visualization tutorials for biomedical
researchers. Every tutorial is a `.qmd` (Quarto Markdown) file rendered by Quarto and
published to GitHub Pages via `babelquarto::render_website()`.

Key directories:
- `<Category>/*.qmd` – Tutorial source files (e.g., `Omics/`, `Distribution/`, `Correlation/`)
- `images/<Category>/` – Static images for each tutorial category
- `files/` – Small data files (< 1 MB); large data hosted externally on Tencent COS
- `.github/workflows/` – CI/CD pipelines
- `_quarto.yml` – Site configuration (bilingual, freeze-enabled)
- `update-dependencies.R` – Scans `.qmd` files and updates `DESCRIPTION` with new packages
- `prelude.R` – Loaded via `_environment` at startup; provides shared helper functions

---

## Development Environment Setup

### 1. Install R with `rig` (Recommended)

[`rig`](https://github.com/r-lib/rig) is the recommended R version manager.

```bash
# macOS (Homebrew)
brew tap r-lib/rig
brew install --cask rig

# Linux (x86_64)
curl -Ls https://github.com/r-lib/rig/releases/latest/download/rig-linux-x86_64-latest.tar.gz \
  | sudo tar xz -C /usr/local

# Windows (winget)
winget install r-lib.rig
```

Install and activate an R version:

```bash
rig install release      # install latest release
rig default release      # set as default
rig list                 # list installed versions
```

### 2. Install Quarto

Download from <https://quarto.org/docs/get-started/> or use the official installer:

```bash
# macOS (Homebrew)
brew install --cask quarto

# Linux
curl -LO https://quarto.org/download/latest/quarto-linux-amd64.deb
sudo dpkg -i quarto-linux-amd64.deb
```

Verify: `quarto --version`

### 3. Install R Package Dependencies

```r
# Install pak (fast dependency resolver)
install.packages("pak")

# Install all project dependencies declared in DESCRIPTION
pak::pak(ask = FALSE)

# Or using devtools
install.packages("devtools")
devtools::install_deps()
```

The project uses `babelquarto` for bilingual rendering:

```r
pak::pak("ropensci/babelquarto")
```

---

## Development Workflow

### Render a Single Tutorial

```bash
# Render one .qmd file (outputs to _site/)
quarto render Omics/VolcanoPlot.qmd

# Render with a specific profile
quarto render Omics/VolcanoPlot.qmd --profile preview
```

Or from R:

```r
quarto::quarto_render("Omics/VolcanoPlot.qmd")
```

### Render the Full Website

The site uses `babelquarto` for bilingual (EN + ZH) rendering:

```r
babelquarto::render_website()
```

> **Note**: Full site rendering is slow. Use `execute: freeze: auto` (already set in
> `_quarto.yml`) so only changed files are re-executed.

To force a full re-render (clear freeze cache first):

```bash
rm -rf _freeze
Rscript -e 'babelquarto::render_website()'
```

### Preview the Site Locally

```bash
quarto preview
```

This starts a local server and auto-reloads on file changes.

### Scan and Update Package Dependencies

After adding new packages to a tutorial, update `DESCRIPTION`:

```bash
Rscript update-dependencies.R
```

---

## Testing and Validation

### 1. Validate a Tutorial File (devtools-style check)

Use `devtools` to check R code in `.qmd` files:

```r
# Check all R code chunks in a specific file
knitr::purl("Omics/VolcanoPlot.qmd", output = tempfile(fileext = ".R"))
source(knitr::purl("Omics/VolcanoPlot.qmd", output = tempfile(fileext = ".R")))
```

Or render and check for errors:

```r
# Render and capture errors
tryCatch(
  quarto::quarto_render("Omics/VolcanoPlot.qmd"),
  error = function(e) message("Render failed: ", e$message)
)
```

### 2. Lint R Code

```r
# Install lintr if needed
if (!requireNamespace("lintr", quietly = TRUE)) install.packages("lintr")

# Lint all R code in a .qmd
lintr::lint("Omics/VolcanoPlot.qmd")
```

### 3. Check Package Installation Guards

Every tutorial's setup chunk must guard all installs:

```r
# Correct pattern
if (!requireNamespace("ggplot2", quietly = TRUE)) install.packages("ggplot2")

# For GitHub-only packages
if (!requireNamespace("SomePkg", quietly = TRUE)) {
  if (!requireNamespace("devtools", quietly = TRUE)) install.packages("devtools")
  devtools::install_github("owner/SomePkg")
}
```

### 4. Verify Bilingual Pairs

Every English `.qmd` must have a matching `.zh.qmd`:

```bash
# Find English files without a Chinese counterpart
for f in **/*.qmd; do
  [[ "$f" == *.zh.qmd ]] && continue
  [[ -f "${f%.qmd}.zh.qmd" ]] || echo "Missing ZH pair: $f"
done
```

### 5. Check for Missing Demo Images

```bash
# Find demo image references without an actual file
grep -r 'images/.*_demo' --include="*.qmd" -h \
  | grep -oP 'images/[^\s\)\"]+' \
  | sort -u \
  | while read img; do
      [ -f "$img" ] || echo "MISSING: $img"
    done
```

### 6. CI Pipeline

The GitHub Actions workflow (`.github/workflows/quarto-publish.yml`):
1. Sets up Quarto and R via `r-lib/actions/setup-r@v2`
2. Runs `update-dependencies.R` to sync `DESCRIPTION`
3. Renders the site with `babelquarto::render_website()`
4. Deploys to GitHub Pages (on push to `main`/`master`)

Trigger a manual re-render via the **Actions** tab → **Github Page** → **Run workflow**.

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
- Remove the `Contributors` line entirely if there are no contributors; do not include an empty Contributors entry (e.g., `"**[Contributors]** "` with no following name).

### 2. Required Sections (in order)

Each tutorial should contain, in this order:

1. **A description paragraph** (immediately after the frontmatter, before any heading).
2. `## Example` – contains a demo image `![](../images/<Category>/<Name>_demo.png){...}`.
3. `## Setup` – lists: System Requirements, Programming Language, Dependencies, and an `{r packages setup}` code block.
4. `## Data Preparation` – loads data, preferably from public biomedical sources.
5. `## Visualization` – contains one or more `{r fig...}` code blocks with `fig-cap` and `label`.

### 3. Code Block Conventions

- The packages setup block must be named: `` ```{r packages setup, message=FALSE, warning=FALSE, output=FALSE} ``
- Figure blocks must use `#| label: fig-<name>`, `#| fig-cap`, and `#| out.width` options; figure labels **must** start with `fig-`.
- Include a `sessioninfo::session_info("attached")` block immediately after the setup block.
- All packages must check for installation before loading (`if (!requireNamespace(...)) install.packages(...)`).
- Code must be self-contained and reproducible using public datasets.
- The **Dependencies** list in `## Setup` must exactly match the packages actually loaded in the setup chunk.

### 4. Bilingual Pairing

- Every English `.qmd` should have a corresponding `.zh.qmd` Chinese translation.
- The auto-translate workflow generates `.zh.qmd` automatically; do not manually edit `.zh.qmd` files unless correcting mistranslations.

### 5. Images

- Demo images go in `images/<Category>/` with the naming convention `<TutorialName>_demo.png`.
- Reference them as `![](../images/<Category>/<TutorialName>_demo.png){fig-alt="..." fig-align="center" width="60%"}`.
- Every tutorial referenced demo image must exist as an actual file before merging.

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
- Description paragraph placed after `## Example` instead of before it.
- Hardcoded local file paths (e.g., `read.csv("/home/user/data.csv")`).
- Direct commit of large binary files or data files > 1 MB.
- Figure code blocks missing `#| label: fig-<name>` (label must start with `fig-`).
- Missing `sessioninfo::session_info("attached")` block.
- Missing bilingual pair (`.zh.qmd`) when adding a new tutorial.
- Packages loaded without the standard `if (!requireNamespace(...))` guard.
- Dependencies listed in `## Setup` that do not match the packages actually installed/loaded.
- Referenced demo image files that do not exist in `images/<Category>/`.
- Empty author entries (e.g., `"**[Contributors]** "` with no name).
