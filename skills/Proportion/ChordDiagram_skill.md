# Skill: Chord Diagram (R)

## Category
Proportion

## When to Use
Chord diagrams can use connecting lines or bars to represent the relationships between different objects. The connections in a chord diagram directly show the relationships between different objects; the width of the connection is proportional to the strength of the relationship, and the color of the connection can represent another mapping of the relationship, such as the type of relationship. The size of the sectors in the diagram represents the measurement of the objects.

## Required R Packages
- chorddiag
- circlize
- dplyr
- ggraph
- htmlwidgets
- igraph
- readr
- readxl
- tidygraph
- tidyverse
- viridis

## Minimal Reproducible Code
```r
# Load packages
library(chorddiag)
library(circlize)
library(dplyr)
library(ggraph)
library(htmlwidgets)
library(igraph)

# Prepare data
# TCGA-BRCA.star_counts.tsv
tcga_brca_star_counts <- readr::read_tsv("https://bizard-1301043367.cos.ap-guangzhou.myqcloud.com/TCGA-BRCA.star_counts.tsv")
target_ensembl_ids <- c("ENSG00000012048.23",  # BRCA1
                        "ENSG00000139618.16",  # BRCA2
                        "ENSG00000141736.14",  # ERBB2
                        "ENSG00000121879.6",  # PIK3CA
                        "ENSG00000171862.11",  # PTEN
                        "ENSG00000111537.5")  # AKT1
gene_data <- tcga_brca_star_counts[tcga_brca_star_counts$Ensembl_ID %in% target_ensembl_ids, ]
gene_data <- gene_data[,2:101]
gene_data_t <- t(gene_data)

x <- c(gene_data_t[, 1], gene_data_t[, 3], gene_data_t[, 5])
y <- c(gene_data_t[, 2], gene_data_t[, 4], gene_data_t[, 6])
factor <- rep(c("a", "b", "c"), each = 100)
plot_data <- data.frame(x = x, y = y, factor = factor)

# Berberine_new
berberine_blood_glucose <- readr::read_csv("https://bizard-1301043367.cos.ap-guangzhou.myqcloud.com/Berberine_new.csv")
berberine_blood_glucose <- berberine_blood_glucose %>% na.omit()

blood_glucose_category <- function(value) {
  if (value < 4.5) {
    return("Low")
  } else if (value >= 4.5 & value < 6.5) {
    return("Normal")
  } else if (value >= 6.5 & value <= 11.0) {
    return("Slightly High")
  } else {
    return("High")
  }
}

berberine_blood_glucose$before_category <- sapply(berberine_blood_glucose$before, blood_glucose_category)
berberine_blood_glucose$after_category <- sapply(berberine_blood_glucose$after, blood_glucose_category)

adj_matrix <- table(berberine_blood_glucose$before_category, berberine_blood_glucose$after_category) # Generate adjacency matrix
adj_matrix_df <- as.data.frame(as.table(adj_matrix))

adj_matrix_wide <- adj_matrix_df %>%
  pivot_wider(names_from = Var2, values_from = Freq, values_fill = list(Freq = 0))
# ... (see full tutorial for more)
```

## Key Parameters
- `alpha`: Controls transparency (0 = fully transparent, 1 = opaque)
- `width`: Controls element width
- `fill`: Maps a variable to fill color for group comparison
- `color`: Maps a variable to outline/point color

## Tips
- The tutorial includes a '4. Highly customized chord diagrams' section with advanced styling options
- Adjust text size with `theme(text = element_text(size = 14))` for presentations
- See the full tutorial for additional customization options and advanced examples

## Full Tutorial
https://openbiox.github.io/Bizard/Proportion/ChordDiagram.html
