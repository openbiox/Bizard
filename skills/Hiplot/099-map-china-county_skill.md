# Skill: China Map (County) (R)

## Category
Hiplot

## When to Use
Create a China Map (County) using R with the Hiplot platform's approach. Suitable for biomedical data visualization with publication-quality output.

## Required R Packages
- RColorBrewer
- data.table
- ggplot2
- jsonlite

## Minimal Reproducible Code
```r
# Load packages
library(RColorBrewer)
library(data.table)
library(ggplot2)
library(jsonlite)

# Prepare data
# Load data
data <- data.table::fread(jsonlite::read_json("https://hiplot.cn/ui/basic/map-china-county/data.json")$exampleData$textarea[[1]])
data <- as.data.frame(data)
dt_map <- readRDS(url("https://download.hiplot.cn/ui/basic/map-china-county/china.county.rds"))

# Convert data structure
dt_map$Value <- data$value[match(dt_map$county, data$name)]

# View data
head(data)

# Create visualization
# China Map (County)
p <- ggplot(dt_map) +
  geom_polygon(aes(x = long, y = lat, group = group, fill = Value),
               alpha = 0.9, size = 0.5) +
  geom_path(aes(x = long, y = lat, group = group), color = "black", size = 0.2) +
  coord_fixed() +
  scale_fill_gradientn(
    colours = colorRampPalette(rev(brewer.pal(11,"RdYlBu")))(500),
    breaks = seq(min(data$value), max(data$value), 
                 round((max(data$value)-min(data$value))/7)),
    name = "",
    guide = guide_legend(
      direction = "vertical", keyheight = unit(1, units = "mm"),
      keywidth = unit(8, units = "mm"),
      title.position = "top", title.hjust = 0.5, label.hjust = 0.5,
      nrow = 1, byrow = T, reverse = F, label.position = "bottom")) +
  theme(text = element_text(color = "#3A3F4A"),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "top",
        legend.text = element_text(size = 4 * 1.5, color = "black"),
        legend.title = element_text(size = 5 * 1.5, color = "black"),
        plot.title = element_text(
          face = "bold", size = 5 * 1.5, hjust = 0.5, 
          margin = margin(t = 4, b = 5), color = "black"),
        plot.background = element_rect(fill = "#FFFFFF", color = "#FFFFFF"),
        panel.background = element_rect(fill = "#FFFFFF", color = NA),
        legend.background = element_rect(fill = "#FFFFFF", color = NA),
        plot.margin = unit(c(1.5, 1.5, 1.5, 1.5), "cm")) +
# ... (see full tutorial for more)
```

## Key Parameters
- `x`: Maps `long` to the x aesthetic
- `y`: Maps `lat` to the y aesthetic
- `group`: Maps `group` to the group aesthetic
- `fill`: Maps `Value` to the fill aesthetic
- `alpha`: Controls transparency (0 = fully transparent, 1 = opaque)
- `width`: Controls element width
- `position`: Position adjustment (identity, dodge, stack, fill)

## Tips
- Customize color scales with `scale_fill_manual()` or `scale_color_brewer()`
- Adjust text size with `theme(text = element_text(size = 14))` for presentations
- See the full tutorial for additional customization options and advanced examples

## Full Tutorial
https://openbiox.github.io/Bizard/Hiplot/099-map-china-county.html
