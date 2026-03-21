# Skill: Multiple Sequences Alignment (R)

## Category
Omics

## When to Use
Multiple Sequence Alignment (MSA) is a fundamental and crucial technique in bioinformatics. It is used to align three or more biological sequences (DNA, RNA, or proteins) based on their evolutionary or structural similarities, so that homologous sites (i.e., sites derived from a common ancestor) are aligned as much as possible.

## Required R Packages
- ggmsa

## Minimal Reproducible Code
```r
# Load packages
library(ggmsa)

# Prepare data
# Example data
protein_fasta <- system.file("extdata", "sample.fasta", package = "ggmsa")

# Data preview
seqs <- readLines(protein_fasta)
head(seqs)

# Create visualization
# Multiple sequence alignment of proteins
p <- ggmsa(
  protein_fasta,
  start = 300,
  end = 330,
  font = "DroidSansMono",
  color = "Chemistry_AA",
  char_width = 0.5,
  seq_name = TRUE,
  consensus_views = FALSE
)

p
```

## Key Parameters
- `width`: Controls element width
- `stat`: Statistical transformation to use
- `fill`: Maps a variable to fill color for group comparison
- `color`: Maps a variable to outline/point color

## Tips
- Adjust text size with `theme(text = element_text(size = 14))` for presentations
- Include appropriate statistical thresholds (e.g., FDR < 0.05, |log2FC| > 1) in the visualization
- See the full tutorial for additional customization options and advanced examples

## Full Tutorial
https://openbiox.github.io/Bizard/Omics/MultiSeqsAlignment.html
