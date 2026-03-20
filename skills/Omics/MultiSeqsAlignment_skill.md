# Skill: Multiple Sequences Alignment (R)

## Category
Omics

## When to use
Multiple Sequence Alignment (MSA) is a fundamental and crucial technique in bioinformatics. It is used to align three or more biological sequences (DNA, RNA, or proteins) based on their evolutionary or structural similarities, so that homologous sites (i.e., sites derived from a common ancestor) are aligned as much as possible.

## Required R packages
- ggmsa

## Minimal reproducible code
```r
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

## Full tutorial
https://openbiox.github.io/Bizard/Omics/MultiSeqsAlignment.html
