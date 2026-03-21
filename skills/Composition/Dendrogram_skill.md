# Skill: Dendrogram (R)

## Category
Composition

## When to Use
A dendrogram is a graphical representation of hierarchical relationships between objects. It is widely used in cluster analysis, especially hierarchical clustering, to visualize the similarity or distance between data points.

## Required R Packages
- collapsibleTree
- dendextend
- ggraph
- igraph
- tidyverse

## Minimal Reproducible Code
```r
# Load packages
library(collapsibleTree)
library(dendextend)
library(ggraph)
library(igraph)
library(tidyverse)

# Prepare data
# warpbreaks
data("warpbreaks")
warpbreaks <- warpbreaks %>%
  mutate(breaks = as.character(breaks))

# Convert nested dataframe data to side list data and then draw a tree diagram.
edges_level1_2 <- warpbreaks %>%
  select(wool, tension) %>%
  distinct() %>%
  rename(from = wool, to = tension)

edges_level2_3 <- warpbreaks %>%
  select(tension, breaks) %>%
  distinct() %>%
  rename(from = tension, to = breaks)

edge_list <- bind_rows(edges_level1_2, edges_level2_3) # merge
edge_list_unique <- edge_list[edge_list$from != "B",]
edge_list_unique$to <- make.unique(edge_list_unique$to)


# Create a graph object
mygraph_unique <- graph_from_data_frame(edge_list_unique)

# Hierarchical grouping
V(mygraph_unique)$group <- case_when(
  V(mygraph_unique)$name %in% unique(warpbreaks$wool) ~ "Group 1", # root node wool
  str_detect(V(mygraph_unique)$name, "^[LMH]") ~ "Group 2", # First layer of tension
  str_detect(V(mygraph_unique)$name, "^[0-9]") ~ "Group 3", # Second layer breaks
  TRUE ~ "Group 4" # Additional correction layer
)
V(mygraph_unique)$color <- case_when(
  V(mygraph_unique)$group == "Group 1" ~ "red",
  V(mygraph_unique)$group == "Group 2" ~ "yellow",
  V(mygraph_unique)$group == "Group 3" ~ "green",
  V(mygraph_unique)$group == "Group 4" ~ "blue"
)

# mtcars
mtcars %>% 
  select(mpg, cyl, disp) %>% 
  dist() %>% 
# ... (see full tutorial for more)
```

## Key Parameters
- `color`: Maps `color` to the color aesthetic
- `alpha`: Controls transparency (0 = fully transparent, 1 = opaque)
- `position`: Position adjustment (identity, dodge, stack, fill)
- `theme`: Plot theme; tutorial uses `theme_void()`

## Tips
- Adjust text size with `theme(text = element_text(size = 14))` for presentations
- Ensure proportions sum to 100% and consider using a colorblind-friendly palette
- See the full tutorial for additional customization options and advanced examples

## Full Tutorial
https://openbiox.github.io/Bizard/Composition/Dendrogram.html
