# Skill: PCA (R)

## Category
Hiplot

## When to use
::: callout-note
**Hiplot website**

## Required R packages
- data.table
- ggplot2
- ggpubr
- gmodels
- jsonlite

## Minimal reproducible code
```r
# PCA
p <- ggplot(pca_data, aes(x = PC1, y = PC2, color = colorBy)) +
  geom_point(size = 4, alpha = 0.8) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray70") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray70") +
  stat_ellipse(level = 0.95, show.legend = FALSE) +
  ggtitle(conf$general$title) +
  labs(
    x = paste0("PC1 (", variance_explained[1], "%)"),
    y = paste0("PC2 (", variance_explained[2], "%)"),
    color = axis[1]
  ) +
  
  # Custom color scheme
  scale_color_brewer(palette = conf$general$palette) +
  
  # Add sample labels
  geom_text(aes(label = sample), 
            hjust = 0.5, vjust = -1, size = 3.5, show.legend = FALSE) +
  
  # Theme settings
  theme_bw(base_size = 12) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
    axis.title = element_text(size = 14, face = "bold"),
    axis.text = element_text(size = 12),
    legend.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 11),
    legend.position = "right",
    panel.grid.major = element_line(color = "grey90", linewidth = 0.3),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(fill = NA, color = "grey50", linewidth = 0.5),
    aspect.ratio = 1
  )

# Display plot
p
```

## Full tutorial
https://openbiox.github.io/Bizard/Hiplot/187-pca.html
