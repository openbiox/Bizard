# Skill: Pie Matrix (R)

## Category
Hiplot

## When to use
::: callout-note
**Hiplot website**

## Required R packages
- data.table
- dplyr
- ggplot2
- jsonlite
- stringr
- tidyr

## Minimal reproducible code
```r
# Pie Matrix
p <- df %>% as.table() %>%
  as.data.frame() %>%
  mutate(Freq = str_split(Freq,",")) %>%
  unnest(Freq) %>%
  mutate(Freq = as.integer(Freq)) %>%
  # Convert the values to a percentage (which adds up to 1 for each graph)
  group_by(Var1, Var2) %>%
  mutate(Freq = ifelse(is.na(Freq), NA, Freq / sum(Freq)),
         color = row_number()) %>%
  ungroup() %>%
  # Plot
  ggplot(aes("", Freq, fill=factor(color, labels = unique(data[,"status"])))) + 
  geom_bar(width = 2, stat = "identity") +
  coord_polar("y") +
  facet_wrap(~Var1+Var2, ncol = ncol(df)) +
  scale_fill_manual(values = col) +
  theme_void() +
  theme(axis.text = element_blank(), axis.ticks = element_blank(),
        panel.grid = element_blank(), axis.title = element_blank(),
        legend.position = "bottom", legend.direction = "horizontal") +
  guides(fill = guide_legend(nrow = 1, title = "status"))
  
p
```

## Full tutorial
https://openbiox.github.io/Bizard/Hiplot/140-pie-matrix.html
