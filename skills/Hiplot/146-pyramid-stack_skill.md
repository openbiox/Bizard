# Skill: Pyramid Stack (R)

## Category
Hiplot

## When to Use
The pyramid stack is a pyramid-like figure that distributes data on both sides of a central axis.

## Required R Packages
- data.table
- dplyr
- ggplot2
- ggthemes
- jsonlite

## Minimal Reproducible Code
```r
# Load packages
library(data.table)
library(dplyr)
library(ggplot2)
library(ggthemes)
library(jsonlite)

# Prepare data
# Load data
data <- data.table::fread(jsonlite::read_json("https://hiplot.cn/ui/basic/pyramid-stack/data.json")$exampleData[[1]]$textarea[[1]])
data <- as.data.frame(data)

# Convert data structure
data[,3] <- factor(data[,3], levels = unique(data[,3]))
data[,1] <- factor(data[,1], levels = unique(data[,1]))

# View data
head(data)

# Create visualization
# Pyramid Stack
p <- ggplot(data = data, aes(x = age, y = pop, fill = year)) +
  geom_bar(data = data %>% filter(gender == "female") %>% arrange(rev(year)),
           stat = "identity", position = "identity") +
  geom_bar(data = data %>% filter(gender == "male") %>% arrange(rev(year)),
           stat = "identity", position = "identity", mapping = aes(y = -pop)) +
  coord_flip() +
  geom_hline(yintercept = 0) +
  scale_fill_economist() +
  scale_fill_manual(values = c("#e04d39","#5bbad6","#1e9f86")) +
  labs(y = "pop | male (left) - female (right)", x= "") +
  theme_economist(horizontal = FALSE) +
  theme(text = element_text(family = "Arial"),
        plot.title = element_text(size = 12,hjust = 0.5),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 10),
        axis.text.x = element_text(angle = 0, hjust = 0.5,vjust = 1),
        legend.position = "top",
        legend.direction = "horizontal",
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 10),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

p
```

## Key Parameters
- `x`: Maps `age` to the x aesthetic
- `y`: Maps `pop` to the y aesthetic
- `fill`: Maps `year` to the fill aesthetic
- `position`: Position adjustment (identity, dodge, stack, fill)
- `stat`: Statistical transformation to use

## Tips
- Use `coord_flip()` for horizontal orientation when labels are long
- Customize color scales with `scale_fill_manual()` or `scale_color_brewer()`
- See the full tutorial for additional customization options and advanced examples

## Full Tutorial
https://openbiox.github.io/Bizard/Hiplot/146-pyramid-stack.html
