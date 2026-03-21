# Skill: Text-Overlaid Enrichment Barplot (R)

## Category
Omics

## When to Use
The Text-Overlaid Enrichment Barplot is a visualization tool designed for the high-density display of functional enrichment analysis results (e.g., GO, KEGG). It typically maps enrichment significance (adjusted p-value) to the length of rounded bars and utilizes the internal space of the graphics to directly overlay annotations of pathway names and core gene lists. Additionally, it uses colored blocks and bubbles on the left side to distinguish functional categories and gene counts.

## Required R Packages
- clusterProfiler
- ggprism
- gground
- org.Hs.eg.db
- tidyverse

## Minimal Reproducible Code
```r
# Load packages
library(clusterProfiler)
library(ggprism)
library(gground)
library(org.Hs.eg.db)
library(tidyverse)

# Prepare data
# 1. Read Data
raw_data <- read_tsv("https://bizard-1301043367.cos.ap-guangzhou.myqcloud.com/DAVID.txt")

# 2. Data Cleaning
# Convert DAVID format to the standard format required for plotting
raw_data <- raw_data %>%
  # 2.1 Extract Category Labels
  mutate(Category = case_when(
    grepl("BP_DIRECT", Category) ~ "BP",
    grepl("CC_DIRECT", Category) ~ "CC",
    grepl("MF_DIRECT", Category) ~ "MF",
    grepl("KEGG_PATHWAY", Category) ~ "KEGG",
    TRUE ~ "Other"
  )) %>%
  # Keep only GO and KEGG results
  filter(Category %in% c("BP", "CC", "MF", "KEGG")) %>%
  
  # 2.2 Clean Pathway Names (Remove IDs, e.g., "GO:001~Name" -> "Name")
  mutate(Description = sub("^.*~|.*:", "", Term)) %>%
  
  # 2.3 Rename Columns (Unified variable names for plotting code)
  # FDR -> p.adjust (Significance)
  # Genes -> geneID (Gene List)
  rename(
    p.adjust = FDR,
    geneID = Genes
  ) %>%
  
  # 2.4 Format Gene Lists (Replace commas with slashes)
  mutate(geneID = gsub(", ", "/", geneID))

# Create visualization
# Define color palette
pal <- c('#eaa052', '#b74147', '#90ad5b', '#23929c')

# Other recommended palettes
#pal <- c('#c3e1e6', '#f3dfb7', '#dcc6dc', '#96c38e')
#pal <- c('#7bc4e2', '#acd372', '#fbb05b', '#ed6ca4')

# Adjust position parameters for left blocks in the simplified version
rect.data.simple <- rect.data %>%
  mutate(
# ... (see full tutorial for more)
```

## Key Parameters
- `y`: Maps `Description` to the y aesthetic
- `x`: Maps `0` to the x aesthetic
- `fill`: Maps `Category` to the fill aesthetic
- `colour`: Maps `Category` to the colour aesthetic
- `alpha`: Controls transparency (0 = fully transparent, 1 = opaque)
- `width`: Controls element width
- `position`: Position adjustment (identity, dodge, stack, fill)
- `theme`: Plot theme; tutorial uses `theme_prism()`

## Tips
- The tutorial includes a '2. Advanced Plotting (Detailed Version)' section with advanced styling options
- Customize color scales with `scale_fill_manual()` or `scale_color_brewer()`
- Include appropriate statistical thresholds (e.g., FDR < 0.05, |log2FC| > 1) in the visualization

## Full Tutorial
https://openbiox.github.io/Bizard/Omics/TextEnrichmentBarPlot.html
