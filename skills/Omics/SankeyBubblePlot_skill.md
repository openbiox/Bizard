# Skill: Sankey Bubble plot (R)

## Category
Omics

## When to Use
Create a Sankey Bubble plot visualization in R for biomedical data analysis and research publications.

## Required R Packages
- ggalluvial
- patchwork
- readr
- tidyverse

## Minimal Reproducible Code
```r
# Load packages
library(ggalluvial)
library(patchwork)
library(readr)
library(tidyverse)

# Prepare data
# Load data
data <- read_tsv("files/DAVID.txt")

# Add a new categorical column
get_category <- function(cat) {
  if (grepl("BP", cat)) return("BP")
  if (grepl("MF", cat)) return("MF")
  if (grepl("CC", cat)) return("CC")
  if (grepl("KEGG", cat)) return("KEGG")
  return(NA)
}
data$MainCategory <- sapply(data$Category, get_category)

# Remove SMART and NA
data2 <- data %>%
  filter(!grepl("SMART", Category)) %>%
  filter(!is.na(MainCategory))

# Sort each category and take the top 10
topN <- function(data, n=10) {
  data %>%
  arrange(desc(Count), PValue) %>%
  head(n)
}
result <- data2 %>%
  group_by(MainCategory) %>%
  group_modify(~topN(.x, 10)) %>%
  ungroup()

# KEGG pathway annotation
result <- result %>%
  mutate(
    Source = ifelse(MainCategory == "KEGG", "KEGG", "GO"),
    KEGG_Group = case_when(
      MainCategory == "KEGG" & str_detect(Term,"Neuro|synapse|neurodegeneration|Alzheimer|Parkinson|Prion") ~ "Nervous system",
      MainCategory == "KEGG" & str_detect(Term, "Cytokine|inflammatory") ~ "Immune system",
      MainCategory == "KEGG" & str_detect(Term, "Lipid|atherosclerosis") ~ "Lipid metabolism",
      MainCategory == "KEGG" ~ "Other KEGG",
      TRUE ~ NA_character_
      ),
    GO_Group = ifelse(MainCategory != "KEGG", MainCategory, NA)
    )
alluvial_data <- result %>%
# ... (see full tutorial for more)
```

## Key Parameters
- `y`: Maps `1` to the y aesthetic
- `fill`: Maps `Group` to the fill aesthetic
- `x`: Maps `min` to the x aesthetic
- `size`: Maps `Count` to the size aesthetic
- `alpha`: Controls transparency (0 = fully transparent, 1 = opaque)
- `width`: Controls element width
- `position`: Position adjustment (identity, dodge, stack, fill)
- `stat`: Statistical transformation to use

## Tips
- Use `theme_minimal()` or `theme_bw()` for clean, publication-ready plots
- Customize color scales with `scale_fill_manual()` or `scale_color_brewer()`
- Include appropriate statistical thresholds (e.g., FDR < 0.05, |log2FC| > 1) in the visualization

## Full Tutorial
https://openbiox.github.io/Bizard/Omics/SankeyBubblePlot.html
