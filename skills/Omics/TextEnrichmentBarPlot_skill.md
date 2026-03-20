# Skill: Text-Overlaid Enrichment Barplot (R)

## Category
Omics

## When to use
The Text-Overlaid Enrichment Barplot is a visualization tool designed for the high-density display of functional enrichment analysis results (e.g., GO, KEGG). It typically maps enrichment significance (adjusted p-value) to the length of rounded bars and utilizes the internal space of the graphics to directly overlay annotations of pathway names and core gene lists. Additionally, it uses colored blocks and bubbles on the left side to distinguish functional categories and gene counts.

## Required R packages
- clusterProfiler
- ggprism
- gground
- org.Hs.eg.db
- tidyverse

## Minimal reproducible code
```r
# Define color palette
pal <- c('#eaa052', '#b74147', '#90ad5b', '#23929c')

# Other recommended palettes
#pal <- c('#c3e1e6', '#f3dfb7', '#dcc6dc', '#96c38e')
#pal <- c('#7bc4e2', '#acd372', '#fbb05b', '#ed6ca4')

# Adjust position parameters for left blocks in the simplified version
rect.data.simple <- rect.data %>%
  mutate(
    xmin = -1.5 * width,
    xmax = -0.5 * width
  )

p1 <- ggplot(use_pathway, aes(-log10(p.adjust), y = index, fill = Category)) +
  # 1. Rounded Bar Chart Body
  geom_round_col(aes(y = Description), width = 0.6, alpha = 0.8) +
  
  # 2. Pathway Name Text
  geom_text(aes(x = 0.05, label = Description), hjust = 0, size = 4) +
  
  # 3. Left Category Blocks
  geom_round_rect(
    aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = Category),
    data = rect.data.simple,
    radius = unit(2, 'mm'),
    inherit.aes = FALSE
  ) +
  
  # 4. Left Category Text
  geom_text(
    aes(x = (xmin + xmax) / 2, y = (ymin + ymax) / 2, label = Category),
    data = rect.data.simple,
    size = 3,
    inherit.aes = FALSE
  ) +
  
  # 5. Bottom Decorative Line
  geom_segment(
    aes(x = 0, y = 0, xend = xaxis_max, yend = 0),
    linewidth = 1,
    inherit.aes = FALSE
  ) +
  
  # 6. Style Adjustments
  labs(y = NULL, x = "-log10(p.adjust)") +
  scale_fill_manual(name = 'Category', values = pal) +
  scale_x_continuous(breaks = seq(0, xaxis_max, 2), expand = expansion(c(0, 0.1))) +
  theme_prism() +
  theme(
    axis.text.y = element_blank(),
    axis.line.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.line.x = element_blank(), # Hide default X-axis line, keeping only the one drawn by geom_segment
    legend.title = element_text(),
    legend.position = "right" 
  )

p1
```

## Full tutorial
https://openbiox.github.io/Bizard/Omics/TextEnrichmentBarPlot.html
