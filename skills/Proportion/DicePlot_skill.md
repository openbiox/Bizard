# Skill: Dice Plot (R)

## Category
Proportion

## When to use
Dice plots are a visualization technique for representing high-dimensional categorical data. The ggdiceplot package provides ggplot2 extensions for creating dice-based visualizations where each dot position on a dice represents a specific categorical variable. This allows intuitive visualization of up to 6 categorical variables simultaneously using traditional dice patterns. Each dice position (1-6) represents a different category, with dots shown only when that category is present.

## Required R packages
- dplyr
- ggdiceplot
- ggplot2

## Minimal reproducible code
```r
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

p1
```

## Full tutorial
https://openbiox.github.io/Bizard/Proportion/DicePlot.html
