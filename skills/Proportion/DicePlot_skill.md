# Skill: Dice Plot (R)

## Category
Proportion

## When to Use
Dice plots are a visualization technique for representing high-dimensional categorical data. The ggdiceplot package provides ggplot2 extensions for creating dice-based visualizations where each dot position on a dice represents a specific categorical variable. This allows intuitive visualization of up to 6 categorical variables simultaneously using traditional dice patterns. Each dice position (1-6) represents a different category, with dots shown only when that category is present.

## Required R Packages
- dplyr
- ggdiceplot
- ggplot2

## Minimal Reproducible Code
```r
# Load packages
library(dplyr)
library(ggdiceplot)
library(ggplot2)

# Prepare data
# Load sample data from package
data("sample_dice_miRNA", package = "ggdiceplot")
df_dice <- sample_dice_miRNA

# View data structure
head(df_dice)

# Check data dimensions
str(df_dice)

# Create visualization
# Define colors for regulation direction
direction_colors <- c(
  Down      = "#2166ac",
  Unchanged = "grey80",
  Up        = "#b2182b"
)

# Create basic dice plot
p1 <- ggplot(df_dice, aes(x = miRNA, y = Compound)) +
  geom_dice(
    aes(
      dots   = Organ,
      fill   = direction,
      width  = 0.8,
      height = 0.8
    ),
    show.legend = TRUE,
    ndots       = length(levels(df_dice$Organ)),
    x_length    = length(levels(df_dice$miRNA)),
    y_length    = length(levels(df_dice$Compound))
  ) +
  scale_fill_manual(values = direction_colors, name = "Regulation") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 0.5),
    axis.text.y = element_text(hjust = 1),
    panel.grid  = element_blank()
  ) +
  labs(
    x = "miRNA",
    y = "Compound"
  )

# ... (see full tutorial for more)
```

## Key Parameters
- `x`: Maps `miRNA` to the x aesthetic
- `y`: Maps `Compound` to the y aesthetic
- `fill`: Maps `direction` to the fill aesthetic
- `width`: Controls element width
- `theme`: Plot theme; tutorial uses `theme_minimal()`

## Tips
- The tutorial includes a '2. Advanced Dice Plot with Continuous Variables' section with advanced styling options
- Use `theme_minimal()` or `theme_bw()` for clean, publication-ready plots
- Customize color scales with `scale_fill_manual()` or `scale_color_brewer()`

## Full Tutorial
https://openbiox.github.io/Bizard/Proportion/DicePlot.html
