# Skill: Volcano Plot (R)

## Category
Omics

## When to use
The volcano plot is used to compare the two groups and obtain the up-regulation/down-regulation between the two groups. The screening basis is the p value and FC value, which are converted to -logP value and log2(FC) value. The imported data can be the OTU table or ASV table of the microbiome, the table of transcriptome gene expression, or the features table of metabolomics and other multi-omics data.

## Required R packages
- ggrepel
- readxl
- tidyverse

## Minimal reproducible code
```r
# Basic volcano plot
p <- 
  ggplot(data, aes(x = log2FC, y = -log10(Pvalue))) + # Plot preliminary volcano
  # Plot scatter points, colored by significant categories
  geom_point(aes(color = significant), alpha = 0.6, size = 1.5) + 
  # Set color mapping (up: red, down: green, no significant: gray)
  scale_color_manual(
    values = c("Upregulated" = "red", "Downregulated" = "green", "Not significant" = "gray"),
    name = "Significance" # Legend Title
    ) +
  # Adding a filter threshold line
  geom_vline(xintercept = c(-2, 2), linetype = "dashed", color = "green", linewidth = 0.5) + # log2FC threshold line
  geom_hline(yintercept = -log10(0.05), linetype = "dashed", color = "blue", linewidth = 0.5) + # p-value threshold line
  # Adjust axes and titles
  labs(x = "log2(Fold Change)", y = "-log10(P-value)",
       title = "Volcano Plot with Thresholds") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"), # Title centered bold
        legend.position = "right") # Legend position

p
```

## Full tutorial
https://openbiox.github.io/Bizard/Omics/VolcanoPlot.html
