# Skill: Density Plot (R)

## Category
Distribution

## When to Use
A density plot represents the distribution of a numerical variable using kernel density estimation to display the probability density function. It is a smoothed version of a histogram, sharing the same concept but providing a clearer representation of the overall trend and shape of the data.

## Required R Packages
- cowplot
- dplyr
- geomtextpath
- ggExtra
- ggplot2
- ggpmisc
- ggpubr
- hrbrthemes
- readr
- tidyr
- viridis

## Minimal Reproducible Code
```r
# Load packages
library(cowplot)
library(dplyr)
library(geomtextpath)
library(ggExtra)
library(ggplot2)
library(ggpmisc)

# Prepare data
# Read the TSV data 
data <- readr::read_csv("https://bizard-1301043367.cos.ap-guangzhou.myqcloud.com/TCGA-LIHC.htseq_counts.csv.gz")

# Filter and reshape data for the first gene TSPAN6 (Ensembl ID: ENSG00000000003.13)
data1 <- data %>%   
  filter(Ensembl_ID == "ENSG00000000003.13") %>%   
  pivot_longer(     
    cols = -Ensembl_ID,      
    names_to = "sample",      
    values_to = "expression"   
    ) %>%   
  mutate(var = "var1")  # Add a column to differentiate the variables  

# Filter and reshape data for the second gene SCYL3 (Ensembl ID: ENSG00000000457.12)
data2 <- data %>%   
  filter(Ensembl_ID == "ENSG00000000457.12") %>%   
  pivot_longer(     
    cols = -Ensembl_ID,      
    names_to = "sample",      
    values_to = "expression"   
    ) %>%   
  mutate(var = "var2")  # Add a column to differentiate the variables  

# Combine the two datasets 
data12 <- bind_rows(data1, data2)  

# View the final combined dataset 
head(data12)

# Create visualization
# Basic Density Plot 
p1 <- ggplot(data1, aes(x = expression)) +   
  geom_density(fill = "#69b3a2", color = "#e9ecef", alpha = 0.8) +   
  labs(title = "Density Plot of TSPAN6 Expression Levels",        
       x = "Expression",        
       y = "Density") +   
  theme_minimal()  
p1
```

## Key Parameters
- `x`: Maps `Petal` to the x aesthetic
- `fill`: Maps `Species` to the fill aesthetic
- `group`: Maps `cut` to the group aesthetic
- `color`: Maps `Species` to the color aesthetic
- `alpha`: Controls transparency (0 = fully transparent, 1 = opaque)
- `width`: Controls element width
- `position`: Position adjustment (identity, dodge, stack, fill)
- `stat`: Statistical transformation to use

## Tips
- Use `theme_minimal()` or `theme_bw()` for clean, publication-ready plots
- Customize color scales with `scale_fill_manual()` or `scale_color_brewer()`
- Use `facet_wrap()` or `facet_grid()` to create multi-panel plots by group
- Consider adding `geom_jitter()` or raw data points alongside distribution plots for small sample sizes

## Full Tutorial
https://openbiox.github.io/Bizard/Distribution/Density.html
