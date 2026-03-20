# Skill: Half Violin (R)

## Category
Hiplot

## When to use
::: callout-note
**Hiplot website**

## Required R packages
- data.table
- dplyr
- ggplot2
- ggpubr
- ggthemes
- jsonlite

## Minimal reproducible code
```r
# Half Violin
geom_flat_violin <- function(
  mapping = NULL, data = NULL, stat = "ydensity", position = "dodge", 
  trim = TRUE, scale = "area", show.legend = NA, inherit.aes = TRUE, ...) {
  ggplot2::layer(data = data, mapping = mapping, stat = stat, 
                 geom = geom_flat_violin_proto, position = position,
                 show.legend = show.legend, inherit.aes = inherit.aes,
                 params = list(trim = trim, scale = scale, ...))
}

"%||%" <- function(a, b) {
  if (!is.null(a)) {
    a
  } else {
    b
  }
}

geom_flat_violin_proto <-
  ggproto("geom_flat_violin_proto", Geom,
          setup_data = function(data, params) {
            data$width <- data$width %||%
              params$width %||% (resolution(data$x, FALSE) * 0.9)
            
            data %>%
              dplyr::group_by(.data = ., group) %>%
              dplyr::mutate(.data = ., ymin = min(y), ymax = max(y), xmin = x,
                            xmax = x + width / 2)
          },
          
          draw_group = function(data, panel_scales, coord) {
            data <- base::transform(data, xminv = x, 
                                    xmaxv = x + violinwidth * (xmax - x))
            
            newdata <- base::rbind(
              dplyr::arrange(.data = base::transform(data, x = xminv), y),
              dplyr::arrange(.data = base::transform(data, x = xmaxv), -y))
            
            newdata <- rbind(newdata, newdata[1, ])
            
            ggplot2:::ggname("geom_flat_violin",
                             GeomPolygon$draw_panel(newdata, panel_scales, coord))
          },
          
          draw_key = draw_key_polygon,
          
          default_aes = ggplot2::aes(weight = 1, colour = "grey20", fill = "white",
                                     size = 0.5, alpha = NA, linetype = "solid"),
          required_aes = c("x", "y")
        )

p <- ggplot(data = data, aes(Group, Value, fill = Group)) +
  geom_flat_violin(alpha = 1, scale = "count", trim = FALSE) +
  geom_boxplot(width = 0.05, fill = "white", alpha = 1, 
               outlier.colour = NA, position = position_nudge(0.05)) +
  stat_summary(fun = mean, geom = "point", fill = "white", shape = 21, size = 2,
               position = position_nudge(0.05)) +
  geom_dotplot(alpha = 1, binaxis = "y", dotsize = 0.5, stackdir = "down", 
               binwidth = 0.1, position = position_nudge(-0.025)) +
  theme(legend.position = "none") +
  xlab(colnames(data)[2]) +
  ylab(colnames(data)[1]) +
  guides(fill = F) +
  ggtitle("Half Violin Plot") +
  scale_fill_manual(values = c("#e04d39","#5bbad6","#1e9f86")) +
  theme_stata() +
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

## Full tutorial
https://openbiox.github.io/Bizard/Hiplot/085-half-violin.html
