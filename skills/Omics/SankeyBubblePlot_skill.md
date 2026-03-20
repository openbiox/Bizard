# Skill: Sankey Bubble plot (R)

## Category
Omics

## When to use
Visualize sankey bubble plot data in a biomedical context.

## Required R packages
- ggalluvial
- patchwork
- readr
- tidyverse

## Minimal reproducible code
```r
# Sankey diagram (the Term column does not display labels)

# Ensure Group is of character type and does not contain NA
alluvial_data$Group <- as.character(alluvial_data$Group)
alluvial_data$Group[is.na(alluvial_data$Group)] <- "Other"
# Calculate the total Count of each Group
group_order <- alluvial_data %>%
  group_by(Group) %>%
  summarise(group_count = sum(Count, na.rm = TRUE)) %>%
  arrange(desc(group_count)) %>%
  pull(Group)
# Set group to an ordered factor
alluvial_data$Group <- factor(alluvial_data$Group, levels = group_order)
# Sort the Term column and set term as an ordered factor
term_order <- alluvial_data %>%
  group_by(Term) %>%
  summarise(total_count = sum(Count, na.rm = TRUE)) %>%
  arrange(desc(total_count)) %>%
  pull(Term)
alluvial_data$Term <- factor(alluvial_data$Term, levels = term_order)
# Retrieve the ordered labels of the group
group_labels <- levels(alluvial_data$Group)
group_labels <- c("BP", "MF", "CC", "Nervous system", "Immune system", "Lipid metabolism", "Other KEGG")
term_labels <- levels(alluvial_data$Term)

p1 <- ggplot(
  alluvial_data,
  aes(axis1 = Source, axis2 = Group, axis3 = Term, y = 1)) +
  geom_alluvium(aes(fill = Group), width = 1/12, alpha = 0.8) +
  geom_stratum(width = 1/12, fill = "grey", color = "black") +
  scale_fill_manual(values = c(
    "BP" = "#33ad37","MF" = "#f2c867","CC" = "#d45327", 
    "Nervous system" = "#2eb6aa", "Immune system" = "#3e4999", 
    "Lipid metabolism" = "#4fc1e4", "Other KEGG" = "#e0c4ce")) +
  geom_text(stat = "stratum", aes(label = ifelse(
    after_stat(stratum) %in% group_labels, after_stat(stratum),
    ifelse(after_stat(stratum) %in% term_labels, after_stat(stratum), "")
    )), size = 3) +
  scale_x_discrete(
    limits = c("Source", "Group", "Term"),
    labels = c("Source", "Group", "term"), expand = c(.05, .05)) +
  labs(title = NULL, y = NULL, x = NULL) +
  theme_minimal(base_size = 12) +
  theme(
    axis.title.x = element_blank(),
    axis.text.x = element_text(size = 10),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    plot.margin = margin(5, 5, 5, 5), # This is consistent with p2
    panel.grid = element_blank()
    ) +
  guides(fill = "none")

p1
```

## Full tutorial
https://openbiox.github.io/Bizard/Omics/SankeyBubblePlot.html
