# Skill: Volcano Plot (R)

## Category
Omics

## When to Use
The volcano plot is used to compare the two groups and obtain the up-regulation/down-regulation between the two groups. The screening basis is the p value and FC value, which are converted to -logP value and log2(FC) value. The imported data can be the OTU table or ASV table of the microbiome, the table of transcriptome gene expression, or the features table of metabolomics and other multi-omics data.

## Required R Packages
- ggrepel
- readxl
- tidyverse

## Minimal Reproducible Code
```r
# Load packages
library(ggrepel)
library(readxl)
library(tidyverse)

# Prepare data
# Load excel data
data <- read_excel("files/volcano.eg.xlsx")

# Rename column names (handle special characters)
data <- data %>%
  rename(log2FC = "log2 Ratio(WT0/LOG)", Pvalue = "Pvalue")
# Handle the case where the p-value is 0 (avoid calculating -Inf)
data <- data %>%
  mutate(log10P = -log10(Pvalue + 1e-300)) # Make sure to handle the case where P=0
# Convert to numeric type and handle values that fail to convert (such as invalid characters)
data <- data %>%
  mutate(
    log2FC = as.numeric(log2FC) # Values that fail the conversion become NA
  )

# Find the original value that caused the conversion to fail
data %>%
  filter(is.na(log2FC)) %>%
  select(log2FC) # View the raw log2FC values for these lines
# Repair the data as needed (e.g. replace or remove outliers)
# Example: Replace "Inf" with an actual value or filter out
data <- data %>%
  mutate(
    log2FC = ifelse(log2FC == "Inf", 100, log2FC), # Adjust according to needs
    log2FC = as.numeric(log2FC)
    ) %>%
  filter(!is.na(log2FC)) # Delete the rows that cannot be repaired

# Defining significance (satisfying both P value < 0.05 and |log2FC| > 1)
# Define significance categories (upregulated, downregulated, not significant)
data <- data %>%
  mutate(
    significant = case_when(
      Pvalue < 0.05 & log2FC > 2 ~ "Upregulated", # Up (red)
      Pvalue < 0.05 & log2FC < -2 ~ "Downregulated", # Down (green)
      TRUE ~ "Not significant" # Not significant (grey)
    )
  )

# View data structure
head(data, 5)

# Create visualization
# Basic volcano plot
# ... (see full tutorial for more)
```

## Key Parameters
- `x`: Maps `log2FC` to the x aesthetic
- `color`: Maps `significant` to the color aesthetic
- `alpha`: Controls transparency (0 = fully transparent, 1 = opaque)
- `width`: Controls element width
- `position`: Position adjustment (identity, dodge, stack, fill)
- `theme`: Plot theme; tutorial uses `theme_minimal()`

## Tips
- Use `theme_minimal()` or `theme_bw()` for clean, publication-ready plots
- Customize color scales with `scale_fill_manual()` or `scale_color_brewer()`
- Include appropriate statistical thresholds (e.g., FDR < 0.05, |log2FC| > 1) in the visualization

## Full Tutorial
https://openbiox.github.io/Bizard/Omics/VolcanoPlot.html
