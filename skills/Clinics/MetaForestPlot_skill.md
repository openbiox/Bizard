# Skill: Meta-Analysis Forest Plot (R)

## Category
Clinics

## When to Use
Create a Meta-Analysis Forest Plot visualization in R for biomedical data analysis and research publications.

## Required R Packages
- dplyr
- forestplot
- ggplot2
- grid
- meta
- metafor
- tidyr

## Minimal Reproducible Code
```r
# Load packages
library(dplyr)
library(forestplot)
library(ggplot2)
library(grid)
library(meta)
library(metafor)

# Prepare data
# Generate simulated data
set.seed(2023)
n_studies <- 15
meta_data <- tibble(
  `Study Name` = paste("Study", LETTERS[1:n_studies]),
  `Odds Ratio` = exp(rnorm(n_studies, mean = 0.2, sd = 0.4)),
  `Lower 95% CI` = exp(rnorm(n_studies, mean = 0.1, sd = 0.35)),
  `Upper 95% CI` = exp(rnorm(n_studies, mean = 0.3, sd = 0.45)),
  `Weight (%)` = runif(n_studies, 0.5, 3),
  `Treatment Group` = sample(c("DrugA", "DrugB"), n_studies, replace = TRUE)
) %>% 
  mutate(
    across(c(`Odds Ratio`, `Lower 95% CI`, `Upper 95% CI`), ~round(., 2)),
    `Weight (%)` = round(`Weight (%)`/sum(`Weight (%)`)*100, 1),
    `Study Name` = factor(`Study Name`, levels = rev(`Study Name`))
  ) 

# View the final merged dataset
head(meta_data)

# Create visualization
# Basic forest plot
p <-
  ggplot(meta_data, aes(x = `Odds Ratio`, y = `Study Name`)) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "grey50") +
  geom_errorbarh(aes(xmin = `Lower 95% CI`, xmax = `Upper 95% CI`), 
                 height = 0.15, color = "#2c7fb8", linewidth = 0.8) +
  geom_point(aes(size = `Weight (%)`), shape = 18, color = "#d95f00") +
  scale_x_continuous(trans = "log", 
                     breaks = c(0.25, 0.5, 1, 2, 4),
                     limits = c(0.2, 5)) +
  labs(x = "Odds Ratio (95% CI)", 
       y = "",
       title = "Meta-Analysis Forest Plot",
       subtitle = "Random Effects Model") +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor.x = element_blank(),
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5, color = "grey50"),
# ... (see full tutorial for more)
```

## Key Parameters
- `y`: Maps `Study_Name` to the y aesthetic
- `x`: Maps `log_OR` to the x aesthetic
- `size`: Maps `Sample_Size` to the size aesthetic
- `fill`: Maps `Effect_Type` to the fill aesthetic
- `width`: Controls element width
- `position`: Position adjustment (identity, dodge, stack, fill)

## Tips
- Use `theme_minimal()` or `theme_bw()` for clean, publication-ready plots
- Customize color scales with `scale_fill_manual()` or `scale_color_brewer()`
- Follow CONSORT or STROBE guidelines for clinical data visualization where applicable

## Full Tutorial
https://openbiox.github.io/Bizard/Clinics/MetaForestPlot.html
