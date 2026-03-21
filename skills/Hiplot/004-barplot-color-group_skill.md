# Skill: Barplot Color Group (R)

## Category
Hiplot

## When to Use
The color group barplot can be used to display data values in groups, and to label different colors in sequence.

## Required R Packages
- data.table
- ggplot2
- jsonlite
- stringr

## Minimal Reproducible Code
```r
# Load packages
library(data.table)
library(ggplot2)
library(jsonlite)
library(stringr)

# Prepare data
# Load data
data <- data.table::fread(jsonlite::read_json("https://hiplot.cn/ui/basic/barplot-color-group/data.json")$exampleData$textarea[[1]])
data <- as.data.frame(data)

# convert data structure
colnames(data) <- c("term", "count", "type")
data[,"term"] <- str_to_sentence(str_remove(data[,"term"], pattern = "\\w+:\\d+\\W"))
data[,"term"] <- factor(data[,"term"], 
                        levels =  data[,"term"][length(data[,"term"]):1])
data[,"type"] <- factor(data[,"type"], 
                        levels = data[!duplicated(data[,"type"]), "type"])

# View data
data

# Create visualization
# Barplot Color Group
p <- ggplot(data = data, aes(x = term, y = count, fill = type)) +
  geom_bar(stat = "identity", width = 0.8) + 
  theme_bw() +
  xlab("Count") +
  ylab("Term") +
  guides(fill = guide_legend(title="Type")) +
  ggtitle("Barplot Color Group") + 
  coord_flip() +
  theme_classic() +
  scale_fill_manual(values = c("#E64B35FF","#4DBBD5FF","#00A087FF","#3C5488FF")) +
  theme(text = element_text(family = "Arial"),
        plot.title = element_text(size = 12,hjust = 0.5),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 10),
        axis.text.x = element_text(angle = 0, hjust = 0.5,vjust = 1),
        legend.position = "right",
        legend.direction = "vertical",
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 10))

p
```

## Key Parameters
- `x`: Maps `term` to the x aesthetic
- `y`: Maps `count` to the y aesthetic
- `fill`: Maps `type` to the fill aesthetic
- `width`: Controls element width
- `position`: Position adjustment (identity, dodge, stack, fill)
- `stat`: Statistical transformation to use
- `theme`: Plot theme; tutorial uses `theme_bw()`

## Tips
- Use `theme_minimal()` or `theme_bw()` for clean, publication-ready plots
- Use `coord_flip()` for horizontal orientation when labels are long
- Customize color scales with `scale_fill_manual()` or `scale_color_brewer()`

## Full Tutorial
https://openbiox.github.io/Bizard/Hiplot/004-barplot-color-group.html
