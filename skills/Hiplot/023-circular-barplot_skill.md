# Skill: Circular Barplot (R)

## Category
Hiplot

## When to Use
Drawing circular barplot

## Required R Packages
- data.table
- dplyr
- ggplot2
- jsonlite

## Minimal Reproducible Code
```r
# Load packages
library(data.table)
library(dplyr)
library(ggplot2)
library(jsonlite)

# Prepare data
# Load data
data <- data.table::fread(jsonlite::read_json("https://hiplot.cn/ui/basic/circular-barplot/data.json")$exampleData$textarea[[1]])
data <- as.data.frame(data)

# convert data structure
data$group <- as.factor(data$group)
empty_bar <- 2
to_add <- data.frame(matrix(NA, empty_bar*nlevels(data$group), ncol(data)))
colnames(to_add) <- colnames(data)
to_add$group <- rep(levels(data$group), each=empty_bar)
data <- rbind(data, to_add)
data <- data %>% arrange(group)
data$id <- seq(1, nrow(data))

label_data <- data
number_of_bar <- nrow(label_data)
angle <- 90 - 360 * (label_data$id-0.5) /number_of_bar
label_data$hjust <- ifelse( angle < -90, 1, 0)
label_data$angle <- ifelse(angle < -90, angle+180, angle)

base_data <- data %>% 
  group_by(group) %>% 
  dplyr::summarize(start=min(id), end=max(id) - empty_bar) %>% 
  rowwise() %>% 
  mutate(title=mean(c(start, end)))

# View data
head(data)

# Create visualization
# Circular Barplot
p <- ggplot(data, aes(x=as.factor(id), y=value, fill=group)) +
  geom_bar(aes(x=as.factor(id), y=value, fill=group), stat="identity", alpha=0.5) +
  ylim(-50,max(na.omit(data$value))+30) +
  geom_segment(data=base_data, aes(x = start, y = -5, xend = end, yend = -5), colour = "black", alpha=0.8, size=0.8 , inherit.aes = FALSE )  +
  geom_text(data=base_data, aes(x = title, y = -12, label=group), colour = "black", alpha=0.8, size=4, fontface="bold", inherit.aes = FALSE) +
  geom_text(data=label_data, aes(x=id, y=value+8, label=individual, hjust=hjust), color="black", fontface="bold",alpha=0.6, size=3, angle= label_data$angle, inherit.aes = FALSE ) +
  geom_text(data=label_data, aes(x=id, y=value-10, label=value, hjust=hjust), color="black", fontface="bold",alpha=0.6, size=3, angle= label_data$angle, inherit.aes = FALSE ) +
  coord_polar() + 
  scale_fill_manual(values = c("#3b4992ff","#ee0000ff","#008b45ff","#631879ff")) +
  theme_minimal() +
  theme(text = element_text(family = "Arial"),
        plot.title = element_text(size = 12,hjust = 0.5),
# ... (see full tutorial for more)
```

## Key Parameters
- `x`: Maps `id` to the x aesthetic
- `y`: Maps `value` to the y aesthetic
- `alpha`: Controls transparency (0 = fully transparent, 1 = opaque)
- `position`: Position adjustment (identity, dodge, stack, fill)
- `stat`: Statistical transformation to use
- `theme`: Plot theme; tutorial uses `theme_minimal()`

## Tips
- Use `theme_minimal()` or `theme_bw()` for clean, publication-ready plots
- Customize color scales with `scale_fill_manual()` or `scale_color_brewer()`
- See the full tutorial for additional customization options and advanced examples

## Full Tutorial
https://openbiox.github.io/Bizard/Hiplot/023-circular-barplot.html
