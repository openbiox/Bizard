# Skill: Meta-Analysis Forest Plot (R)

## Category
Clinics

## When to use
Visualize meta-analysis forest plot data in a biomedical context.

## Required R packages
- dplyr
- forestplot
- ggplot2
- grid
- meta
- metafor
- tidyr

## Minimal reproducible code
```r
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
    legend.position = "bottom"
  )
p
```

## Full tutorial
https://openbiox.github.io/Bizard/Clinics/MetaForestPlot.html
