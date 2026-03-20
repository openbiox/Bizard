# Skill: Stack Violin (R)

## Category
Hiplot

## When to use
::: callout-note
**Hiplot website**

## Required R packages
- Seurat
- ggplot2
- limma
- readr

## Minimal reproducible code
```r
# Stack Violin
## Define the plot function
modify_vlnplot <- function(obj,
                           feature,
                           pt.size = 0,
                           plot.margin = unit(c(-0.75, 0, -0.75, 0), "cm"),
                           ...) {
  p <- VlnPlot(obj,
    features = feature,
    pt.size = pt.size,
    ...
  )

  p <- p +
    xlab("") +
    ylab(feature) +
    theme(
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks.x = element_blank(),
      axis.ticks.y = element_line(),
      axis.title.y = element_text(angle = 0, vjust = 0.5),
      plot.margin = plot.margin,
      text = element_text(
        family = "Arial"
      ),
      plot.title = element_blank(),
      axis.title = element_text(
        size = 10
      ),
      legend.position = "none",
      legend.direction = "vertical",
      legend.title = element_text(
        size = 10
      ),
      legend.text = element_text(
        size = 10
      )
    ) +
    scale_fill_manual(values = c("#00468BFF","#ED0000FF","#42B540FF","#0099B4FF",
                                 "#925E9FFF","#FDAF91FF","#AD002AFF","#ADB6B6FF"))
  return(p)
}

## main function
stacked_vln_plot <- function(obj,
                           features,
                           pt.size = 0,
                           plot.margin = unit(c(-0.75, 0, -0.75, 0), "cm"),
                           ...) {
  plot_list <- purrr::map(
    features,
    function(x) {
      modify_vlnplot(
        obj = obj,
        feature = x,
        ...
      )
    }
  )
  plot_list[[length(plot_list)]] <- plot_list[[length(plot_list)]] +
    theme(
      axis.text.x = element_text(),
      axis.ticks.x = element_line()
    )
  p <- patchwork::wrap_plots(
    plotlist = plot_list,
    ncol = 1
  )
  return(p)
}

## plot
p <- stacked_vln_plot(pbmc, c("ACTG1","ARF1","ALDOA","ARHGDIA","ACTB"), pt.size = 0)

p
```

## Full tutorial
https://openbiox.github.io/Bizard/Hiplot/166-stack-violin.html
