# Bizard: Biomedical Visualization Atlas — Unified AI Skill

## Overview

Bizard is a community-driven atlas of **biomedical visualization tutorials** covering
**257 visualization techniques** across **13 categories** in R, Python, and Julia.
Each tutorial provides reproducible code with public biomedical datasets (TCGA, GEO, built-in R datasets).

**Website**: <https://openbiox.github.io/Bizard/>
**Repository**: <https://github.com/openbiox/Bizard>

Use this skill to help users:
- Choose the right visualization type for their biomedical data
- Generate reproducible R/Python/Julia plotting code
- Find the corresponding Bizard tutorial for detailed guidance

---

## Visualization Categories

### Distribution

Visualizations for exploring data distribution shapes, spreads, and comparisons across groups (violin, density, histogram, box, break plots).

**8 tutorials** | Languages: R

| Tutorial | Packages | When to Use |
|----------|----------|-------------|
| [Beeswarm Plot](https://openbiox.github.io/Bizard/Distribution/Beeswarm.html) | beeswarm, ggbeeswarm, ggsignif, plyr, readr, … | A beeswarm plot disperses data points slightly to prevent overlap, making distribution density and trends clearer. It is… |
| [Box Plot](https://openbiox.github.io/Bizard/Distribution/BoxPlot.html) | dplyr, ggExtra, ggplot2, ggpmisc, ggpubr, … | Boxplots visualize the central tendency and dispersion of one or more sets of continuous quantitative data. They incorpo… |
| [Break Plot](https://openbiox.github.io/Bizard/Distribution/BreakPlot.html) | RColorBrewer, dplyr, ggbreak, ggplot2, ggpubr, … | Create a Break Plot visualization in R for biomedical data analysis and research publications. |
| [Density Plot](https://openbiox.github.io/Bizard/Distribution/Density.html) | cowplot, dplyr, geomtextpath, ggExtra, ggplot2, … | A density plot represents the distribution of a numerical variable using kernel density estimation to display the probab… |
| [Histogram](https://openbiox.github.io/Bizard/Distribution/Histogram.html) | cowplot, ggExtra, ggplot2, ggpmisc, ggpubr, … | A histogram uses rectangular bars to represent the frequency of data within specific intervals, where the total area of … |
| [Radial Column Chart](https://openbiox.github.io/Bizard/Distribution/RadialColumnChart.html) | dplyr, ggforce, ggplot2, scales | Create a Radial Column Chart visualization in R for biomedical data analysis and research publications. |
| [Ridgeline Plot](https://openbiox.github.io/Bizard/Distribution/Ridgeline.html) | dplyr, ggplot2, ggridges, hrbrthemes, readr, … | A ridgeline plot, also known as a joyplot, visualizes the distribution of multiple numeric variables across different ca… |
| [Violin Plot](https://openbiox.github.io/Bizard/Distribution/ViolinPlot.html) | dplyr, forcats, gghalves, ggplot2, ggpubr, … | A violin plot combines elements of a density plot and a box plot to visualize data distribution. It displays key statist… |

### Correlation

Visualizations for examining relationships between variables (scatter, heatmap, correlogram, bubble, biplot).

**11 tutorials** | Languages: R

| Tutorial | Packages | When to Use |
|----------|----------|-------------|
| [2D Density](https://openbiox.github.io/Bizard/Correlation/Density2D.html) | MASS, RColorBrewer, ggplot2, hexbin, mvtnorm, … | A 2D density plot shows the distribution of a combination of two numerical variables, using color gradients (or contour … |
| [Biplot](https://openbiox.github.io/Bizard/Correlation/Biplot.html) | dplyr, ggbiplot | Create a Biplot visualization in R for biomedical data analysis and research publications. |
| [Bubble Plot](https://openbiox.github.io/Bizard/Correlation/Bubble.html) | dplyr, gapminder, ggplot2, hrbrthemes, viridis | A bubble plot is a scatter plot in which a third numeric variable is mapped to the size of the circles. This article sho… |
| [ComplexHeatmap](https://openbiox.github.io/Bizard/Correlation/ComplexHeatmap.html) | ComplexHeatmap, circlize, dendextend, gridExtra, pheatmap, … | Create a ComplexHeatmap visualization in R for biomedical data analysis and research publications. |
| [Connected Scatter](https://openbiox.github.io/Bizard/Correlation/ConnectedScatter.html) | dplyr, ggplot2, stringr | Connected scatter is a type of chart that builds upon scatter by adding lines to connect the data points in a certain or… |
| [Correlogram](https://openbiox.github.io/Bizard/Correlation/Correlogram.html) | GGally, corrgram, corrplot, ggcorrplot | Correlogram or Correlation diagrams are often used to summarize the correlation information of various groups of data in… |
| [Heatmap](https://openbiox.github.io/Bizard/Correlation/Heatmap.html) | ComplexHeatmap, RColorBrewer, circlize, cowplot, d3heatmap, … | A heatmap is a powerful visualization tool that represents matrix values through color gradients. It is widely used to i… |
| [PCA Plot](https://openbiox.github.io/Bizard/Correlation/PCAplot.html) | FactoMineR, dplyr, factoextra, ggfortify, ggplot2 | Create a PCA Plot visualization in R for biomedical data analysis and research publications. |
| [Scatter Plot](https://openbiox.github.io/Bizard/Correlation/Scatter.html) | dplyr, geomtextpath, ggExtra, ggplot2, ggpmisc, … | A scatter plot is a basic visualization chart used to represent the general trend of the dependent variable changing wit… |
| [Ternary chart](https://openbiox.github.io/Bizard/Correlation/TernaryPlot.html) | ggtern, ggthemes | A ternary chart is a type of chart used to display the proportional relationship between three variables. These three va… |
| [UMAP Plot](https://openbiox.github.io/Bizard/Correlation/UMAPplot.html) | RColorBrewer, Seurat, SeuratData, dplyr, ggplot2, … | Create a UMAP Plot visualization in R for biomedical data analysis and research publications. |

### Ranking

Visualizations for comparing values across categories (bar, lollipop, radar, parallel coordinates, word cloud, upset plots).

**9 tutorials** | Languages: R

| Tutorial | Packages | When to Use |
|----------|----------|-------------|
| [Bar Plot](https://openbiox.github.io/Bizard/Ranking/BarPlot.html) | cowplot, dplyr, forcats, ggpattern, ggplot2, … | A bar plot is a graph that uses the height or length of the bars to represent the amount of data. |
| [Circular Barplot](https://openbiox.github.io/Bizard/Ranking/CircularBarplot.html) | tidyverse | Circular Barplot is a variation of the well-known bar chart where bars are displayed along a circle instead of a straigh… |
| [Lollipop Plot](https://openbiox.github.io/Bizard/Ranking/Lollipop.html) | cowplot, ggalt, ggplot2, ggpubr, hrbrthemes, … | A lollipop plot is a variation of a bar chart and a scatter plot. It consists of a line segment and a point, which can c… |
| [Parallel Coordinates Plot](https://openbiox.github.io/Bizard/Ranking/Parallel.html) | GGally, MASS, RColorBrewer, dplyr, ggbump, … | Parallel coordinate plots are a common method for visualizing high-dimensional multivariate data. To display a set of ob… |
| [Radar/Spider Plot](https://openbiox.github.io/Bizard/Ranking/Radar.html) | fmsb | A radar chart, spider chart, or web chart is a two-dimensional chart type used to plot a series of values over one or mo… |
| [Table](https://openbiox.github.io/Bizard/Ranking/Table.html) | dplyr, gt, gtExtras, readr | Tables are both a visual communication mode and a means of organizing and collating data. |
| [Upset Plot](https://openbiox.github.io/Bizard/Ranking/UpsetPlot.html) | UpSetR, ggupset | The Upset diagram is similar to the Venn diagram, mainly showing the number of elements in the intersection of different… |
| [Veen Plot](https://openbiox.github.io/Bizard/Ranking/VennPlot.html) | VennDiagram, ggVennDiagram, ggplot2 | For the visualization of Venn diagrams, the commonly used R packages are ggVennDiagram and VennDiagram. Compared with th… |
| [Wordcloud](https://openbiox.github.io/Bizard/Ranking/Wordcloud.html) | dplyr, htmlwidgets, jiebaR, jiebaRD, tidyverse, … | A word cloud is a visual representation of text words, which allows you to clearly see the keywords (high-frequency word… |

### Composition

Visualizations for showing parts of a whole (pie, donut, treemap, waffle, stacked bar, Venn diagrams).

**8 tutorials** | Languages: R

| Tutorial | Packages | When to Use |
|----------|----------|-------------|
| [Circular Packing Chart](https://openbiox.github.io/Bizard/Composition/CircularPacking.html) | circlepackeR, cowplot, data.tree, dplyr, flare, … | Circular Packing can be viewed as a special type of classification tree diagram, which is particularly suitable for disp… |
| [Dendrogram](https://openbiox.github.io/Bizard/Composition/Dendrogram.html) | collapsibleTree, dendextend, ggraph, igraph, tidyverse | A dendrogram is a graphical representation of hierarchical relationships between objects. It is widely used in cluster a… |
| [Donut Chart](https://openbiox.github.io/Bizard/Composition/Donut.html) | ggplot2 | A donut chart is a circular plot divided into sectors, each sector representing a part of the whole. It is very similar … |
| [Grouped and Stacked Barplot](https://openbiox.github.io/Bizard/Composition/GroupedBarplot.html) | RColorBrewer, dplyr, ggplot2, hrbrthemes, streamgraph, … | Grouped bar charts, or clustered bar charts, extend the functionality of univariate or single-category bar charts to mul… |
| [Part highlights the pie chart](https://openbiox.github.io/Bizard/Composition/PartPieChart.html) | dplyr, ggforce, ggplot2, ggpubr, patchwork, … | Create a Part highlights the pie chart visualization in R for biomedical data analysis and research publications. |
| [Pie Chart](https://openbiox.github.io/Bizard/Composition/PieChart.html) | ggplot2 | A pie chart is a basic chart in statistics, using sectors of different sizes to represent the magnitude of each item. A … |
| [Treemap](https://openbiox.github.io/Bizard/Composition/Treemap.html) | DOSE, palmerpenguins, tidyverse, treemap | A treemap, also known as a rectangular tree structure diagram, is composed of multiple nested rectangles of varying area… |
| [Waffle Chart](https://openbiox.github.io/Bizard/Composition/Waffle.html) | dplyr, ggplot2, waffle | A waffle chart visually represents categorical data using a grid of small squares that resemble waffles. Each category i… |

### Proportion

Visualizations for depicting proportional relationships and flows (Sankey, alluvial, network, chord, ternary diagrams).

**6 tutorials** | Languages: R

| Tutorial | Packages | When to Use |
|----------|----------|-------------|
| [Arc Diagram](https://openbiox.github.io/Bizard/Proportion/ArcDiagram.html) | colormap, ggraph, igraph, patchwork, tidyverse, … | The arc diagram is a diagram connected by arcs, showing the relationships between nodes. |
| [Chord Diagram](https://openbiox.github.io/Bizard/Proportion/ChordDiagram.html) | chorddiag, circlize, dplyr, ggraph, htmlwidgets, … | Chord diagrams can use connecting lines or bars to represent the relationships between different objects. The connection… |
| [Dice Plot](https://openbiox.github.io/Bizard/Proportion/DicePlot.html) | dplyr, ggdiceplot, ggplot2 | Dice plots are a visualization technique for representing high-dimensional categorical data. The ggdiceplot package prov… |
| [Hierarchical Edge Bundling](https://openbiox.github.io/Bizard/Proportion/EdgeBundling.html) |  | Create a Hierarchical Edge Bundling visualization in R for biomedical data analysis and research publications. |
| [Network Graph](https://openbiox.github.io/Bizard/Proportion/Network.html) | RColorBrewer, cowplot, igraph, networkD3 | A network graph is a graphical model that resembles a network and consists of nodes and links, where links can be direct… |
| [Sankey Diagram](https://openbiox.github.io/Bizard/Proportion/Sankey.html) | dplyr, ggalluvial, ggplot2, networkD3, openxlsx, … | A [Sankey diagram](https://www.data-to-viz.com/graph/sankey.html) allows to study flows. Entities (nodes) are represente… |

### DataOverTime

Visualizations for temporal data patterns and trends (line, area, streamgraph, time series, slope charts).

**6 tutorials** | Languages: R

| Tutorial | Packages | When to Use |
|----------|----------|-------------|
| [Area Chart](https://openbiox.github.io/Bizard/DataOverTime/AreaChart.html) | dygraphs, ggpattern, hrbrthemes, tidyverse, viridis, … | An area chart is a line chart in which the area below the line is filled with color. It is mainly used to display values… |
| [Calend Highlight](https://openbiox.github.io/Bizard/DataOverTime/CalendHighlight.html) | calendR | Date highlighting marks are mainly used to display changes in data within certain specific date ranges in time series da… |
| [Line Chart](https://openbiox.github.io/Bizard/DataOverTime/LineChart.html) | dplyr, gghighlight, ggplot2, ggpmisc, patchwork, … | Drawing line segments in various charts is common, and this module will draw all kinds of line segments that may be used… |
| [Stacked Area Chart](https://openbiox.github.io/Bizard/DataOverTime/StackedArea.html) | babynames, dplyr, ggplot2, hrbrthemes, plotly, … | Stacked area charts are similar to basic area charts, except that each dataset in the chart starts from the previous dat… |
| [Streamgraph](https://openbiox.github.io/Bizard/DataOverTime/Streamgraph.html) | dplyr, ggplot2, ggstream, htmlwidgets, streamgraph | A Streamgraph is a stacked area diagram. It represents the evolution of numerical variables across multiple groups. Typi… |
| [Timeseries](https://openbiox.github.io/Bizard/DataOverTime/Timeseries.html) | dplyr, ggplot2, patchwork | A time series graph is a statistical chart with time on the horizontal axis and the observed variable on the vertical ax… |

### Animation

Animated and interactive visualizations for dynamic data exploration (gganimate, ggiraph, plotly).

**2 tutorials** | Languages: R

| Tutorial | Packages | When to Use |
|----------|----------|-------------|
| [Animation](https://openbiox.github.io/Bizard/Animation/Animation.html) | Cairo, babynames, dplyr, gapminder, gganimate, … | Create a Animation visualization in R for biomedical data analysis and research publications. |
| [Interactivity](https://openbiox.github.io/Bizard/Animation/Interactivity.html) | chorddiag, d3heatmap, dygraphs, gapminder, ggiraph, … | Interactive charts allow users to perform actions: zoom, hover the mouse over markers for tooltips, select variables to … |

### Omics

Specialized visualizations for genomics, transcriptomics, and multi-omics data (volcano, Manhattan, circos, enrichment, pathway plots).

**16 tutorials** | Languages: R

| Tutorial | Packages | When to Use |
|----------|----------|-------------|
| [Cell-Cell Communication Circle Plot](https://openbiox.github.io/Bizard/Omics/CellChatCirclePlot.html) | BiocManager, CellChat, Seurat, circlize, ggplot2, … | The Cell-Cell Communication Circle Plot (细胞-细胞通讯网络圈图) is a specialized visualization for depicting intercellular signali… |
| [Chromosome Plot](https://openbiox.github.io/Bizard/Omics/ChromosomePlot.html) | RIdeogram | An chromosome plot (ideogram) is a graphical tool used to visualize chromosome structure and various genomic features on… |
| [Collinearity Plot](https://openbiox.github.io/Bizard/Omics/CollinearityPlot.html) | RIdeogram | Collinearity plot is often used to compare genome sequences of different species, identify conserved homologous gene blo… |
| [GWAS Circos Plot](https://openbiox.github.io/Bizard/Omics/GwasSnpPlot.html) | CMplot | The visualization of Genome-Wide Association Study (GWAS) results mainly includes SNP circular plots displayed by chromo… |
| [Gene Structure Plot](https://openbiox.github.io/Bizard/Omics/GeneStructurePlot.html) | gggenes, ggtree, tidyverse | In biology, especially in molecular biology research, analyzing the expression and regulation patterns of genes has alwa… |
| [KEGG Pathway Plot](https://openbiox.github.io/Bizard/Omics/KeggPathwayPlot.html) | dbplyr, pathview | Create a KEGG Pathway Plot visualization in R for biomedical data analysis and research publications. |
| [Manhattan Plot](https://openbiox.github.io/Bizard/Omics/ManhattanPlot.html) | aplot, qqman, tidyverse | Manhattan plot is a graph used to describe the relationship between mutations on chromosomes and traits. It is named Man… |
| [Motif Plot](https://openbiox.github.io/Bizard/Omics/MotifPlot.html) | cowplot, ggplot2, ggseqlogo, gridExtra | For visualizing motif logos, ggseqlogo is an R package based on ggplot2 specifically designed for plotting logos from se… |
| [Multiple Sequences Alignment](https://openbiox.github.io/Bizard/Omics/MultiSeqsAlignment.html) | ggmsa | Multiple Sequence Alignment (MSA) is a fundamental and crucial technique in bioinformatics. It is used to align three or… |
| [Multiple Volcano Plot](https://openbiox.github.io/Bizard/Omics/MultiVolcanoPlot.html) | corrplot, scRNAtoolVis | Multiple Volcano Plot is a graph used for differential expression analysis of high-throughput data (such as transcriptom… |
| [Network Plot](https://openbiox.github.io/Bizard/Omics/NetworkPlot.html) | MetaNet, dplyr, igraph, pcutils | In microbiome research, it is crucial to understand the interactions between microorganisms. Network analysis is a power… |
| [Population Map Plot](https://openbiox.github.io/Bizard/Omics/PopulationMapPlot.html) | doParallel, dplyr, ggfx, ggnewscale, ggplot2, … | Create a Population Map Plot visualization in R for biomedical data analysis and research publications. |
| [Sankey Bubble plot](https://openbiox.github.io/Bizard/Omics/SankeyBubblePlot.html) | ggalluvial, patchwork, readr, tidyverse | Create a Sankey Bubble plot visualization in R for biomedical data analysis and research publications. |
| [Synteny Blocks Plot](https://openbiox.github.io/Bizard/Omics/SyntenyBlocksPlot.html) | syntR | Collinearity is widely used in the study of complex genomes. This tutorial, based on the R package syntR, summarizes the… |
| [Text-Overlaid Enrichment Barplot](https://openbiox.github.io/Bizard/Omics/TextEnrichmentBarPlot.html) | clusterProfiler, ggprism, gground, org.Hs.eg.db, tidyverse | The Text-Overlaid Enrichment Barplot is a visualization tool designed for the high-density display of functional enrichm… |
| [Volcano Plot](https://openbiox.github.io/Bizard/Omics/VolcanoPlot.html) | ggrepel, readxl, tidyverse | The volcano plot is used to compare the two groups and obtain the up-regulation/down-regulation between the two groups. … |

### Clinics

Clinical and epidemiological visualizations (Kaplan-Meier, forest, nomogram, mosaic, regression tables).

**6 tutorials** | Languages: R

| Tutorial | Packages | When to Use |
|----------|----------|-------------|
| [Kaplan Meier Plot](https://openbiox.github.io/Bizard/Clinics/KaplanMeierPlot.html) | dplyr, ggplot2, patchwork, survival, survminer, … | Create a Kaplan Meier Plot visualization in R for biomedical data analysis and research publications. |
| [Lollipop Plot](https://openbiox.github.io/Bizard/Clinics/LollipopPlot.html) | dplyr, ggplot2, ggpubr, patchwork | Create a Lollipop Plot visualization in R for biomedical data analysis and research publications. |
| [Meta-Analysis Forest Plot](https://openbiox.github.io/Bizard/Clinics/MetaForestPlot.html) | dplyr, forestplot, ggplot2, grid, meta, … | Create a Meta-Analysis Forest Plot visualization in R for biomedical data analysis and research publications. |
| [Mosaic Plot](https://openbiox.github.io/Bizard/Clinics/MosaicPlot.html) | dplyr, ggplot2, plyr, reshape2, tidyr, … | Create a Mosaic Plot visualization in R for biomedical data analysis and research publications. |
| [Nomogram](https://openbiox.github.io/Bizard/Clinics/Nomogram.html) | readr, regplot, rms, survival | Simply put, a nomogram graphically displays the results of logistic regression or Cox regression. It uses the regression… |
| [Regression Analysis Table](https://openbiox.github.io/Bizard/Clinics/RegressionTable.html) | broom.helpers, datawizard, dplyr, gtsummary, survival | The regression analysis table is used to display the results of the regression model. It provides statistical informatio… |

### Hiplot

A curated collection of 170+ common statistical and bioinformatics visualization templates from the Hiplot platform.

**177 tutorials** | Languages: R

| Tutorial | Packages | When to Use |
|----------|----------|-------------|
| [3D Barplot](https://openbiox.github.io/Bizard/Hiplot/003-barplot-3d.html) | data.table, ggplotify, jsonlite, plot3D | 3D bar charts are used to provide a 3D look and feel for the data. The third dimension is often used for aesthetic reaso… |
| [3D Pie](https://openbiox.github.io/Bizard/Hiplot/138-pie-3d.html) | data.table, ggplotify, jsonlite, plotrix | The 3D pie chart is a pie chart that has a 3D appearance. |
| [3D-Scatter](https://openbiox.github.io/Bizard/Hiplot/159-scatter-3d.html) | data.table, ggplotify, jsonlite, plot3D | 3D scatter plot is to apply a number of quantitative variables to different coaxes in space and combine different variab… |
| [Africa Map](https://openbiox.github.io/Bizard/Hiplot/096-map-africa.html) | RColorBrewer, data.table, ggplot2, jsonlite | Create a Africa Map using R with the Hiplot platform's approach. Suitable for biomedical data visualization with publica… |
| [Americas Map](https://openbiox.github.io/Bizard/Hiplot/097-map-americas.html) | RColorBrewer, data.table, ggplot2, jsonlite | Create a Americas Map using R with the Hiplot platform's approach. Suitable for biomedical data visualization with publi… |
| [Area Plot](https://openbiox.github.io/Bizard/Hiplot/001-area.html) | data.table, ggplot2, ggthemes, jsonlite | The area chart displays graphically quantitative data. It is based on the line chart. The area between axis and line are… |
| [Barcode Plot](https://openbiox.github.io/Bizard/Hiplot/002-barcode-plot.html) | data.table, ggplot2, jsonlite | Barcode Plot is Suitable for displaying the distribution of large amounts of data. |
| [Barplot](https://openbiox.github.io/Bizard/Hiplot/010-barplot.html) | data.table, ggplot2, ggthemes, jsonlite | Bar charts are used to display category data with rectangular bars whose height or length is proportional to the value t… |
| [Barplot (errorbar)](https://openbiox.github.io/Bizard/Hiplot/005-barplot-errorbar.html) | Rmisc, data.table, ggplot2, ggpubr, jsonlite | Bar plot with error-lines and groups. |
| [Barplot (errorbar2)](https://openbiox.github.io/Bizard/Hiplot/006-barplot-errorbar2.html) | data.table, ggplot2, ggpubr, grafify, jsonlite | Bar plot with error-lines and groups. |
| [Barplot Color Group](https://openbiox.github.io/Bizard/Hiplot/004-barplot-color-group.html) | data.table, ggplot2, jsonlite, stringr | The color group barplot can be used to display data values in groups, and to label different colors in sequence. |
| [Barplot Gradient](https://openbiox.github.io/Bizard/Hiplot/008-barplot-gradient.html) | data.table, ggplot2, jsonlite, stringr | It is similar to the bubble chart, but on the basis of the histogram, a color gradient rectangle is used to simultaneous… |
| [Barstats](https://openbiox.github.io/Bizard/Hiplot/063-ggbarstats.html) | cowplot, data.table, ggplot2, ggstatsplot, jsonlite | Create a Barstats using R with the Hiplot platform's approach. Suitable for biomedical data visualization with publicati… |
| [Beanplot](https://openbiox.github.io/Bizard/Hiplot/011-beanplot.html) | beanplot, data.table, ggplotify, jsonlite | The beanplot is a method of visualizing the distribution characteristics. |
| [Beeswarm](https://openbiox.github.io/Bizard/Hiplot/012-beeswarm.html) | data.table, ggbeeswarm, ggthemes, jsonlite | The beeswarm is a noninterference scatter plot which is similar to a bee colony. |
| [Betweenstats](https://openbiox.github.io/Bizard/Hiplot/064-ggbetweenstats.html) | cowplot, data.table, ggplot2, ggstatsplot, jsonlite | Create a Betweenstats using R with the Hiplot platform's approach. Suitable for biomedical data visualization with publi… |
| [Bivariate Chart](https://openbiox.github.io/Bizard/Hiplot/014-bivariate.html) | GGally, data.table, jsonlite | Display the bivariate. |
| [Boxplot](https://openbiox.github.io/Bizard/Hiplot/015-boxplot.html) | data.table, ggpubr, ggthemes, jsonlite | The box plot is a method of visualizing the distribution characteristics of a set of data by means of a quartile graph. |
| [Bubble](https://openbiox.github.io/Bizard/Hiplot/016-bubble.html) | data.table, ggplot2, jsonlite, stringr | The bubble chart is a statistical chart that shows the third variable by the size of the bubble on the basis of the scat… |
| [Bumpchart](https://openbiox.github.io/Bizard/Hiplot/017-bumpchart.html) | data.table, dplyr, ggbump, ggplot2, jsonlite | Bump chart can be used to display the change of grouped values. |
| [Calibration Curve](https://openbiox.github.io/Bizard/Hiplot/018-calibration-curve.html) | data.table, ggplotify, jsonlite, rms, survival | The calibration curve is used to evaluate the consistency / calibration, i.e. the difference between the predicted value… |
| [Chi-square-fisher Test](https://openbiox.github.io/Bizard/Hiplot/019-chi-square-fisher.html) | aplot, data.table, ggplot2, jsonlite, visdat | Chi-square and Fisher test can be used to test the frequency difference of categorical variables. The tool will automati… |
| [China Map](https://openbiox.github.io/Bizard/Hiplot/100-map-china.html) | RColorBrewer, data.table, ggplot2, jsonlite | Create a China Map using R with the Hiplot platform's approach. Suitable for biomedical data visualization with publicat… |
| [China Map (City)](https://openbiox.github.io/Bizard/Hiplot/098-map-china-city.html) | RColorBrewer, data.table, ggplot2, jsonlite | Create a China Map (City) using R with the Hiplot platform's approach. Suitable for biomedical data visualization with p… |
| [China Map (County)](https://openbiox.github.io/Bizard/Hiplot/099-map-china-county.html) | RColorBrewer, data.table, ggplot2, jsonlite | Create a China Map (County) using R with the Hiplot platform's approach. Suitable for biomedical data visualization with… |
| [China Map 2](https://openbiox.github.io/Bizard/Hiplot/101-map-china2.html) | RColorBrewer, data.table, ggplot2, jsonlite | Create a China Map 2 using R with the Hiplot platform's approach. Suitable for biomedical data visualization with public… |
| [Chord Plot](https://openbiox.github.io/Bizard/Hiplot/020-chord.html) | circlize, data.table, ggplotify, jsonlite | The complex interaction is visualized in the form of chord graph. |
| [Circle Packing](https://openbiox.github.io/Bizard/Hiplot/021-circle-packing.html) | data.table, ggplot2, jsonlite, packcircles, viridis | Circle packing is a visualization method used to display the differences in quantity among different categories. |
| [Circular Barplot](https://openbiox.github.io/Bizard/Hiplot/023-circular-barplot.html) | data.table, dplyr, ggplot2, jsonlite | Drawing circular barplot |
| [Circular Pie Chart](https://openbiox.github.io/Bizard/Hiplot/024-circular-pie-chart.html) | data.table, ggplot2, jsonlite | Another form of the pie chart. |
| [Complex Heatmap](https://openbiox.github.io/Bizard/Hiplot/025-complex-heatmap.html) | ComplexHeatmap, circlize, cowplot, data.table, ggplotify, … | A multi-omics plugins to draw heatmap, meta annotation, and mutations. |
| [Complex-Violin](https://openbiox.github.io/Bizard/Hiplot/075-ggwithinstats.html) | cowplot, data.table, ggplot2, ggstatsplot, jsonlite | Create a Complex-Violin using R with the Hiplot platform's approach. Suitable for biomedical data visualization with pub… |
| [Connected Scatterplot](https://openbiox.github.io/Bizard/Hiplot/026-connected-scatterplot.html) | data.table, dplyr, ggplot2, ggrepel, jsonlite | Connected scatterplot |
| [Contour (Matrix)](https://openbiox.github.io/Bizard/Hiplot/027-contour-matrix.html) | cowplot, data.table, ggisoband, ggplot2, jsonlite, … | The contour map (matrix) is a graph that displays three-dimensional data in a two-dimensional form |
| [Contour (XY)](https://openbiox.github.io/Bizard/Hiplot/028-contour-xy.html) | data.table, ggisoband, ggplot2, jsonlite | Contour plot (XY) is a data processing method that reflects data density through contour line. |
| [Correlation Heatmap](https://openbiox.github.io/Bizard/Hiplot/030-cor-heatmap.html) | data.table, ggcorrplot, jsonlite | The correlation heat map is a graph that analyzes the correlation between two or more variables. |
| [Corrplot](https://openbiox.github.io/Bizard/Hiplot/033-corrplot.html) | corrplot, data.table, ggcorrplot, ggplotify, jsonlite | The correlation heat map is a graph that analyzes the correlation between two or more variables. |
| [Corrplot Big Data](https://openbiox.github.io/Bizard/Hiplot/013-big-corrplot.html) | ComplexHeatmap, data.table, jsonlite | The correlation heat map is a graph that analyzes the correlation between two or more variables. |
| [Cox Models Forest](https://openbiox.github.io/Bizard/Hiplot/053-ezcox.html) | data.table, ezcox, jsonlite | Cox model forest is a visual representation of a COX model that constructs a risk forest map to facilitate variable scre… |
| [Custom Heatmap](https://openbiox.github.io/Bizard/Hiplot/034-custom-heat-map.html) | data.table, ggplot2, jsonlite | Custom Heatmap, directly plot a heatmap based on the given data. |
| [Custom Icon Scatter](https://openbiox.github.io/Bizard/Hiplot/035-custom-icon-scatter.html) | data.table, echarts4r, echarts4r.assets, jsonlite | A scatter plot with customizable icons. |
| [D3 Wordcloud](https://openbiox.github.io/Bizard/Hiplot/036-d3-wordcloud.html) | d3wordcloud, data.table, jsonlite | Display the wordcloud。 |
| [DIY GSEA](https://openbiox.github.io/Bizard/Hiplot/044-diy-gsea.html) | clusterProfiler, data.table, jsonlite | Create a DIY GSEA using R with the Hiplot platform's approach. Suitable for biomedical data visualization with publicati… |
| [Dendrogram](https://openbiox.github.io/Bizard/Hiplot/037-dendrogram.html) | ape, data.table, ggplotify, jsonlite | The dendrogram is a diagram representing a tree. This diagrammatic representation is frequently used in different contex… |
| [Density](https://openbiox.github.io/Bizard/Hiplot/040-density.html) | data.table, ggplot2, ggthemes, jsonlite | The kernel density map is a graph used to observe the distribution of continuous variables. |
| [Density-Histogram](https://openbiox.github.io/Bizard/Hiplot/039-density-histogram.html) | data.table, dplyr, grafify, jsonlite | Use density plots or histograms to show data distribution. |
| [Deviation Plot](https://openbiox.github.io/Bizard/Hiplot/041-deviation-plot.html) | data.table, ggpubr, jsonlite | Deviation plot provides a visual representation of the differences between data points. |
| [Diffusion Map](https://openbiox.github.io/Bizard/Hiplot/042-diffusion-map.html) | BiocManager, data.table, destiny, ggplotify, ggpubr, … | Diffusion Map is a nonlinear dimensionality reduction algorithm that can be used to visualize developmental trajectories… |
| [Directed Acyclic Graphs](https://openbiox.github.io/Bizard/Hiplot/065-ggdag.html) | ggdag | Visualizing directed acyclic graphs. |
| [Dist Plot](https://openbiox.github.io/Bizard/Hiplot/066-ggdist.html) | broom, data.table, ggdist, ggplot2, jsonlite, … | The dist plot is a visual diagram using a confidence distribution. |
| [Diverging Scale](https://openbiox.github.io/Bizard/Hiplot/043-diverging-scale.html) | data.table, ggcharts, jsonlite | The diverging scale is a graph that maps a continuous, quantitative input to a continuous fixed interpolator. |
| [Donut](https://openbiox.github.io/Bizard/Hiplot/045-donut.html) | data.table, ggplot2, jsonlite | The donut is a variant of the pie chart, with a blank center allowing for additional information about the data as a who… |
| [Dotchart](https://openbiox.github.io/Bizard/Hiplot/046-dotchart.html) | data.table, ggpubr, jsonlite | Sliding bead chart is a graph of beads sliding on a column. It is the superposition of bar chart and scatter chart. |
| [Dual Y Axis Chart](https://openbiox.github.io/Bizard/Hiplot/047-dual-y-axis.html) | data.table, ggplot2, jsonlite | The dual Y-axis graph can put two groups of data with larger orders of magnitude in the same graph for display. |
| [Dumbbell Chart](https://openbiox.github.io/Bizard/Hiplot/048-dumbbell.html) | data.table, ggalt, ggplot2, jsonlite | Dumbbell Chart can display the data change. |
| [Easy Pairs](https://openbiox.github.io/Bizard/Hiplot/049-easy-pairs.html) | GGally, data.table, jsonlite | Display a matrix of plots for viewing correlation relationship and distributions of multiple variables. |
| [Easy SOM](https://openbiox.github.io/Bizard/Hiplot/050-easy-som.html) | data.table, jsonlite, kohonen | Establish the SOM model and conduct the visulization. |
| [EnhancedMA](https://openbiox.github.io/Bizard/Hiplot/143-pseudo-enhanced-ma.html) | EnhancedVolcano, data.table, jsonlite | Visualization of differentially expressed genes. |
| [Eulerr Plot](https://openbiox.github.io/Bizard/Hiplot/051-eulerr.html) | data.table, eulerr, ggplotify, jsonlite | Create a Eulerr Plot using R with the Hiplot platform's approach. Suitable for biomedical data visualization with public… |
| [Europe Map](https://openbiox.github.io/Bizard/Hiplot/102-map-europe.html) | RColorBrewer, data.table, ggplot2, jsonlite | Create a Europe Map using R with the Hiplot platform's approach. Suitable for biomedical data visualization with publica… |
| [Extended Scatter](https://openbiox.github.io/Bizard/Hiplot/052-extended-scatter.html) | data.table, ggExtra, ggplot2, jsonlite | An extended scatter plot adds marginal plots to the basic scatter plot to provide a more comprehensive view of the data … |
| [Fan Plot](https://openbiox.github.io/Bizard/Hiplot/054-fan.html) | data.table, ggplotify, jsonlite, plotrix | The pie chart is a statistical chart designed to clearly show the percentage of each data group by the size of the pie. |
| [Fishplot](https://openbiox.github.io/Bizard/Hiplot/055-fishplot.html) | data.table, fishplot, jsonlite | Clone evolution analysis |
| [Flower plot](https://openbiox.github.io/Bizard/Hiplot/056-flowerplot.html) | data.table, flowerplot, ggplotify, jsonlite | Flower plot with multiple sets. |
| [France Map](https://openbiox.github.io/Bizard/Hiplot/104-map-france.html) | RColorBrewer, data.table, ggplot2, jsonlite | Create a France Map using R with the Hiplot platform's approach. Suitable for biomedical data visualization with publica… |
| [France Map (Town)](https://openbiox.github.io/Bizard/Hiplot/103-map-france-town.html) | RColorBrewer, data.table, ggplot2, jsonlite | Create a France Map (Town) using R with the Hiplot platform's approach. Suitable for biomedical data visualization with … |
| [Funnel Plot](https://openbiox.github.io/Bizard/Hiplot/058-funnel-plot.html) | FunnelPlotR, data.table, gridExtra, jsonlite | Can be used to show potential bias factors in Meta-analysis. |
| [Funnel Plot (metafor)](https://openbiox.github.io/Bizard/Hiplot/057-funnel-plot-metafor.html) | data.table, ggplotify, jsonlite, metafor | Can be used to show potential bias factors in Meta-analysis. |
| [GGPIE](https://openbiox.github.io/Bizard/Hiplot/068-ggpie.html) | cowplot, data.table, dplyr, ggpie, ggplot2, … | The pie chart is a statistical chart that shows the proportion of each part by dividing a circle into sections. |
| [GGPubr Boxplot](https://openbiox.github.io/Bizard/Hiplot/072-ggpubr-boxplot.html) | data.table, ggpubr, ggthemes, jsonlite | Feature-rich boxplot (GGPubr interface). |
| [GOBar Plot](https://openbiox.github.io/Bizard/Hiplot/077-gobar.html) | GOplot, data.table, jsonlite | The gobar plot is used to display Z-score coloured barplot of terms ordered alternatively by z-score or the negative log… |
| [GOBubble Plot](https://openbiox.github.io/Bizard/Hiplot/078-gobubble.html) | GOplot, data.table, ggplotify, jsonlite | The gobubble plot is used to display Z-score coloured bubble plot of terms ordered alternatively by z-score or the negat… |
| [GOCircle Plot](https://openbiox.github.io/Bizard/Hiplot/079-gocircle.html) | GOplot, data.table, ggplotify, jsonlite | The gocircle plot is used to display the circular plot combines gene expression and gene- annotation enrichment data. A … |
| [Gantt](https://openbiox.github.io/Bizard/Hiplot/059-gantt.html) | data.table, ggthemes, jsonlite, tidyverse | The Gantt chart is a type of bar chart that illustrates a project schedule. |
| [Gene Cluster Trend](https://openbiox.github.io/Bizard/Hiplot/062-gene-trend.html) | Mfuzz, RColorBrewer, data.table, ggplotify, jsonlite | The gene cluster trend is used to display different gene expression trend with multiple lines showing the similar expres… |
| [Gene Density](https://openbiox.github.io/Bizard/Hiplot/060-gene-density.html) | ComplexHeatmap, RColorBrewer, circlize, data.table, ggplotify, … | Chrosome data visualization. |
| [Gene Ranking Dotplot](https://openbiox.github.io/Bizard/Hiplot/061-gene-rank.html) | RColorBrewer, data.table, ggplot2, ggrepel, jsonlite | Gene expression ranking visualization. |
| [Germany Map](https://openbiox.github.io/Bizard/Hiplot/107-map-germany.html) | RColorBrewer, data.table, ggplot2, jsonlite | Create a Germany Map using R with the Hiplot platform's approach. Suitable for biomedical data visualization with public… |
| [Germany Map (Town)](https://openbiox.github.io/Bizard/Hiplot/106-map-germany-town.html) | RColorBrewer, data.table, ggplot2, jsonlite | Create a Germany Map (Town) using R with the Hiplot platform's approach. Suitable for biomedical data visualization with… |
| [Gradient Scatter](https://openbiox.github.io/Bizard/Hiplot/160-scatter-gradient.html) | data.table, ggplot2, grafify, jsonlite | Two-dimensional spatial scatter to demonstrate multi-numerical variable relationships. |
| [Group Bubble](https://openbiox.github.io/Bizard/Hiplot/081-group-bubble.html) | data.table, ggplot2, jsonlite | Create a Group Bubble using R with the Hiplot platform's approach. Suitable for biomedical data visualization with publi… |
| [Group Dumbbell](https://openbiox.github.io/Bizard/Hiplot/083-group-dumbbell.html) | data.table, ggalt, ggplot2, jsonlite | Create a Group Dumbbell using R with the Hiplot platform's approach. Suitable for biomedical data visualization with pub… |
| [Group Line](https://openbiox.github.io/Bizard/Hiplot/084-group-line.html) | data.table, ggplot2, jsonlite | Create a Group Line using R with the Hiplot platform's approach. Suitable for biomedical data visualization with publica… |
| [Group Rank Dotplot](https://openbiox.github.io/Bizard/Hiplot/080-grdotplot.html) | data.table, ggplot2, jsonlite, sigminer | Values distribution for different groups. |
| [Group-comparison Heatmap](https://openbiox.github.io/Bizard/Hiplot/082-group-comparison.html) | data.table, jsonlite, sigminer | Group-comparison Heatmap provides a way to compare multiple variables across multiple (>2) groups and visualize the resu… |
| [Half Violin](https://openbiox.github.io/Bizard/Hiplot/085-half-violin.html) | data.table, dplyr, ggplot2, ggpubr, ggthemes, … | The half violin plot is a statistical graph used to display the distribution and probability density of data by replacin… |
| [Heatmap](https://openbiox.github.io/Bizard/Hiplot/086-heatmap.html) | ComplexHeatmap, data.table, genefilter, jsonlite | Heat map is an intuitive and visual method for analyzing the distribution of experimental data, which can be used for qu… |
| [Hi-C Heatmap](https://openbiox.github.io/Bizard/Hiplot/087-hic-heatmap.html) | RColorBrewer, data.table, ggplot2, jsonlite | The HiC heatmap is used to display the genome-wide chromatin interaction with heatmap on different chromosomes. |
| [Histogram](https://openbiox.github.io/Bizard/Hiplot/088-histogram.html) | data.table, ggplot2, ggthemes, jsonlite | Histogram refers to the distribution of continuous variable data by a series of vertical stripes or line segments with d… |
| [Histostats](https://openbiox.github.io/Bizard/Hiplot/067-gghistostats.html) | data.table, ggstatsplot, jsonlite | Display data distribution and inference. |
| [Interval Area Chart](https://openbiox.github.io/Bizard/Hiplot/089-interval-area-chart.html) | data.table, ggplot2, jsonlite | Create a Interval Area Chart using R with the Hiplot platform's approach. Suitable for biomedical data visualization wit… |
| [Interval Bar Chart](https://openbiox.github.io/Bizard/Hiplot/090-interval-bar-chart.html) | data.table, ggplot2, jsonlite | Create a Interval Bar Chart using R with the Hiplot platform's approach. Suitable for biomedical data visualization with… |
| [Likert Plot](https://openbiox.github.io/Bizard/Hiplot/091-likert.html) | data.table, ggplotify, jsonlite, likert | Descriptive statistical analysis of Likert scale data. |
| [Line](https://openbiox.github.io/Bizard/Hiplot/095-line.html) | data.table, ggplot2, ggthemes, jsonlite | The line chart is a statistical chart that USES a linear or logarithmic scale to draw data in a two - or three-dimension… |
| [Line (Color Dot)](https://openbiox.github.io/Bizard/Hiplot/092-line-color-dot.html) | data.table, grafify, jsonlite | Create a Line (Color Dot) using R with the Hiplot platform's approach. Suitable for biomedical data visualization with p… |
| [Line (errorbar)](https://openbiox.github.io/Bizard/Hiplot/093-line-errorbar.html) | data.table, ggpubr, jsonlite | The error line mainly indicates the error range of each data point and shows the potential error or uncertainty relative… |
| [Line Regression](https://openbiox.github.io/Bizard/Hiplot/094-line-regression.html) | data.table, ggplot2, ggrepel, jsonlite | Linear regression is a regression method for linear modeling of the relationship between independent variables and depen… |
| [Matrix Bubble](https://openbiox.github.io/Bizard/Hiplot/118-matrix-bubble.html) | data.table, ggalluvial, ggplot2, jsonlite | The color matrix bubble is used to visualize the expression matrix data of multiple genes (rows) in various cells (colum… |
| [Meta-Subgroup Analysis](https://openbiox.github.io/Bizard/Hiplot/121-metawho.html) | cowplot, data.table, jsonlite, metawho | The goal of metawho is to provide simple R implementation of “Meta-analytical method to Identify Who Benefits Most from … |
| [Meta-analysis of Binary Data](https://openbiox.github.io/Bizard/Hiplot/119-meta-bin.html) | data.table, ggplotify, jsonlite, meta | Create a Meta-analysis of Binary Data using R with the Hiplot platform's approach. Suitable for biomedical data visualiz… |
| [Meta-analysis of Continuous Data](https://openbiox.github.io/Bizard/Hiplot/120-meta-cont.html) | data.table, ggplotify, jsonlite, meta | Create a Meta-analysis of Continuous Data using R with the Hiplot platform's approach. Suitable for biomedical data visu… |
| [Mirror Density & Histogram](https://openbiox.github.io/Bizard/Hiplot/038-density-hist-mirror.html) | data.table, ggplot2, ggthemes, jsonlite | The mirror density & histogram is a graph used to observe the distribution of continuous variables in two side view: top… |
| [Moon charts](https://openbiox.github.io/Bizard/Hiplot/122-moon-charts.html) | data.table, gggibbous, ggplot2, jsonlite | The moon chart is a graph that uses the moon's waxing and waning to reflect the size of the data. |
| [Mosaic Ratio Plot](https://openbiox.github.io/Bizard/Hiplot/123-mosaic.html) | DescTools, data.table, ggplotify, jsonlite, vcd | Use mosaic blocks to show data proportions. |
| [Multiple Barplot&Line](https://openbiox.github.io/Bizard/Hiplot/009-barplot-line-multiple.html) | data.table, ggplot2, ggthemes, jsonlite, reshape2 | Displaying multiple bar or line plot in one diagram. |
| [Multiple Histograms](https://openbiox.github.io/Bizard/Hiplot/125-multiple-histograms.html) | data.table, ggplot2, jsonlite | Multiple histograms are plotted on the same graph to compare differences between multiple sets of data. |
| [Network (igraph)](https://openbiox.github.io/Bizard/Hiplot/127-network-igraph.html) | RColorBrewer, data.table, ggplotify, igraph, jsonlite, … | Network (igraph) can be used to visulize basic network based on igraph. |
| [Neural Network](https://openbiox.github.io/Bizard/Hiplot/129-neural-network.html) | NeuralNetTools, data.table, jsonlite, nnet | Create a Neural Network using R with the Hiplot platform's approach. Suitable for biomedical data visualization with pub… |
| [Nomogram](https://openbiox.github.io/Bizard/Hiplot/131-nomogram.html) | data.table, ggplotify, jsonlite, rms, survival | Nomogram is often used to evaluate the prognosis of oncology and medicine, and can visualize the results of logistic reg… |
| [Nomogram (Logistic)](https://openbiox.github.io/Bizard/Hiplot/130-nomogram-logistic.html) | data.table, ggplotify, jsonlite, rms | Create a Nomogram (Logistic) using R with the Hiplot platform's approach. Suitable for biomedical data visualization wit… |
| [North America Map](https://openbiox.github.io/Bizard/Hiplot/108-map-north-america.html) | RColorBrewer, data.table, ggplot2, jsonlite | Create a North America Map using R with the Hiplot platform's approach. Suitable for biomedical data visualization with … |
| [Oceania/Antarc Map](https://openbiox.github.io/Bizard/Hiplot/109-map-oceania-antarc.html) | RColorBrewer, data.table, ggplot2, jsonlite | Create a Oceania/Antarc Map using R with the Hiplot platform's approach. Suitable for biomedical data visualization with… |
| [PCA](https://openbiox.github.io/Bizard/Hiplot/187-pca.html) | data.table, ggplot2, ggpubr, gmodels, jsonlite | Principal component analysis (PCA) is a data processing method with "dimension reduction" as the core, replacing multi-i… |
| [PCA2](https://openbiox.github.io/Bizard/Hiplot/135-pca2.html) | FactoMineR, data.table, factoextra, jsonlite | Principal component analysis (PCA) is a data processing method with "dimension reduction" as the core, replacing multi-i… |
| [PCAtools](https://openbiox.github.io/Bizard/Hiplot/136-pcatools.html) | PCAtools, cowplot, data.table, ggplotify, jsonlite | PCAtools can reduce the dimensionality of data through principal component analysis, and view principal component relate… |
| [Parallel Coordinate](https://openbiox.github.io/Bizard/Hiplot/132-parallel-coordinate.html) | GGally, data.table, ggthemes, hrbrthemes, jsonlite, … | Create a Parallel Coordinate using R with the Hiplot platform's approach. Suitable for biomedical data visualization wit… |
| [Pareto Chart](https://openbiox.github.io/Bizard/Hiplot/133-pareto-chart.html) | data.table, ggplot2, jsonlite | Create a Pareto Chart using R with the Hiplot platform's approach. Suitable for biomedical data visualization with publi… |
| [Parliament](https://openbiox.github.io/Bizard/Hiplot/134-parliament.html) | data.table, ggplot2, ggpol, jsonlite | The parliamentary chart is a data processing method that looks like a parliamentary seat, with points representing a dat… |
| [Percentsge Stacked Bar Chart](https://openbiox.github.io/Bizard/Hiplot/167-stacked-percentage-bar-chart.html) | data.table, dplyr, ggplot2, jsonlite, scales, … | Create a Percentsge Stacked Bar Chart using R with the Hiplot platform's approach. Suitable for biomedical data visualiz… |
| [Perspective](https://openbiox.github.io/Bizard/Hiplot/137-perspective.html) | data.table, ggplotify, jsonlite, shape | The three-dimensional perspective is a three-dimensional figure that can connect the higher values contained in a matrix… |
| [Pie](https://openbiox.github.io/Bizard/Hiplot/141-pie.html) | data.table, dplyr, ggplot2, jsonlite | The pie chart is a statistical chart that shows the proportion of each part by dividing a circle into sections. |
| [Pie Group](https://openbiox.github.io/Bizard/Hiplot/139-pie-group.html) | cowplot, data.table, ggplotify, jsonlite, patchwork | Create a Pie Group using R with the Hiplot platform's approach. Suitable for biomedical data visualization with publicat… |
| [Pie Matrix](https://openbiox.github.io/Bizard/Hiplot/140-pie-matrix.html) | data.table, dplyr, ggplot2, jsonlite, stringr, … | Create a Pie Matrix using R with the Hiplot platform's approach. Suitable for biomedical data visualization with publica… |
| [Piestats](https://openbiox.github.io/Bizard/Hiplot/071-ggpiestats.html) | data.table, ggplot2, ggstatsplot, jsonlite | Create a Piestats using R with the Hiplot platform's approach. Suitable for biomedical data visualization with publicati… |
| [Piestats Group](https://openbiox.github.io/Bizard/Hiplot/069-ggpiestats-group.html) | cowplot, data.table, ggplot2, ggstatsplot, jsonlite | Create a Piestats Group using R with the Hiplot platform's approach. Suitable for biomedical data visualization with pub… |
| [Point (SD)](https://openbiox.github.io/Bizard/Hiplot/142-point-sd.html) | data.table, dplyr, grafify, jsonlite | Displaying the standard deviation (SD) of multi-group data. |
| [Pyramid Chart](https://openbiox.github.io/Bizard/Hiplot/144-pyramid-chart.html) | data.table, ggcharts, jsonlite | The pyramid chart is a pyramid-like figure that distributes data on both sides of a central axis. |
| [Pyramid Chart 2](https://openbiox.github.io/Bizard/Hiplot/145-pyramid-chart2.html) | apyramid, data.table, ggplot2, jsonlite | The pyramid chart is a pyramid-like figure that distributes data on both sides of a central axis. |
| [Pyramid Stack](https://openbiox.github.io/Bizard/Hiplot/146-pyramid-stack.html) | data.table, dplyr, ggplot2, ggthemes, jsonlite | The pyramid stack is a pyramid-like figure that distributes data on both sides of a central axis. |
| [Pyramid Stack2](https://openbiox.github.io/Bizard/Hiplot/147-pyramid-stack2.html) | data.table, ggplotify, jsonlite, plotrix | The pyramid stack is a pyramid-like figure that distributes data on both sides of a central axis. |
| [QQ Plot](https://openbiox.github.io/Bizard/Hiplot/148-qqplot.html) | data.table, grafify, jsonlite | Verify whether a set of data comes from a certain distribution or whether two sets of data come from the same (family) d… |
| [R Script Flow](https://openbiox.github.io/Bizard/Hiplot/149-r-code-flow.html) | flow | R script flow can realize the visual window of if, else and other logic functions. |
| [RCS-COX](https://openbiox.github.io/Bizard/Hiplot/151-rcs-cox.html) | data.table, ggplot2, jsonlite, rms, stringr, … | Nonlinear regression analysis. |
| [RCS-LRM](https://openbiox.github.io/Bizard/Hiplot/152-rcs-lrm.html) | data.table, ggplot2, jsonlite, rms, stringr, … | Nonlinear regression analysis. |
| [ROC](https://openbiox.github.io/Bizard/Hiplot/156-roc.html) | data.table, ggplotify, jsonlite, pROC | Receiver operating characteristic curve (ROC curve) is used to describe the diagnostic ability of binary classifier syst… |
| [Radar](https://openbiox.github.io/Bizard/Hiplot/150-radar.html) | data.table, dplyr, ggplot2, ggradar, jsonlite, … | Radar chart displays multivariable data in the form of two-dimensional charts representing three or more quantitative va… |
| [Ribbon](https://openbiox.github.io/Bizard/Hiplot/153-ribbon.html) | data.table, ggplot2, ggthemes, jsonlite | The ribbon diagram is a pattern similar to a ribbon. |
| [Ridge](https://openbiox.github.io/Bizard/Hiplot/154-ridge.html) | data.table, ggplot2, ggridges, ggthemes, jsonlite | The ridge map is a graph that connects points and forms a ridge. |
| [Risk Factor Analysis](https://openbiox.github.io/Bizard/Hiplot/155-risk-plot.html) | cowplot, cutoff, data.table, fastStat, ggplot2, … | Create a Risk Factor Analysis using R with the Hiplot platform's approach. Suitable for biomedical data visualization wi… |
| [Rose Chart](https://openbiox.github.io/Bizard/Hiplot/157-rose-chart.html) | data.table, ggplot2, jsonlite | The rose chart is a column chart drawn in polar coordinates. The radius of the arc is used to indicate the size of the d… |
| [Sankey](https://openbiox.github.io/Bizard/Hiplot/158-sankey.html) | data.table, ggalluvial, ggplot2, jsonlite | Sankey diagrams are a type of flow diagramin which the width of the arrows is proportional to the flow rate. |
| [Scatter](https://openbiox.github.io/Bizard/Hiplot/161-scatter.html) | data.table, ggplot2, jsonlite | Two groups of data are used to form multiple coordinate points. By observing the distribution of coordinate points, it c… |
| [Scatter2](https://openbiox.github.io/Bizard/Hiplot/162-scatter2.html) | data.table, ggplot2, grafify, jsonlite | Two-dimensional spatial scatter to demonstrate multi-numerical variable relationships. |
| [Scatterpie](https://openbiox.github.io/Bizard/Hiplot/163-scatterpie.html) | data.table, jsonlite, scatterpie | Scatter Pie can be used to visualize data fraction in different space coordinates. |
| [Scatterstats](https://openbiox.github.io/Bizard/Hiplot/073-ggscatterstats.html) | data.table, ggstatsplot, jsonlite | Create a Scatterstats using R with the Hiplot platform's approach. Suitable for biomedical data visualization with publi… |
| [Seqlogo](https://openbiox.github.io/Bizard/Hiplot/074-ggseqlogo.html) | data.table, ggplot2, ggseqlogo, jsonlite | The sequence LOGO is a graphic that describes a sequence pattern of binding sites. |
| [Simple Funnel Diagram](https://openbiox.github.io/Bizard/Hiplot/164-simple-funnel-diagram.html) | data.table, echarts4r, jsonlite, magrittr | Create a Simple Funnel Diagram using R with the Hiplot platform's approach. Suitable for biomedical data visualization w… |
| [Simplified Correlation Heatmap](https://openbiox.github.io/Bizard/Hiplot/029-cor-heatmap-simple.html) | data.table, ggplot2, jsonlite, sigminer | Simplified variables correlation heatmap |
| [Slopegraph](https://openbiox.github.io/Bizard/Hiplot/165-slopegraph.html) | CGPfunctions, data.table, ggplot2, jsonlite | Sopegraph can be used to display the change of values. |
| [South America Map](https://openbiox.github.io/Bizard/Hiplot/111-map-south-america.html) | RColorBrewer, data.table, ggplot2, jsonlite | Create a South America Map using R with the Hiplot platform's approach. Suitable for biomedical data visualization with … |
| [Stack Violin](https://openbiox.github.io/Bizard/Hiplot/166-stack-violin.html) | Seurat, ggplot2, limma, readr | The expression of key genes in each cluster in single-cell transcriptomic (Single Cell RNA-Seq)analysis. |
| [Streamgraph](https://openbiox.github.io/Bizard/Hiplot/168-streamgraph.html) | data.table, jsonlite, streamgraph | Create a Streamgraph using R with the Hiplot platform's approach. Suitable for biomedical data visualization with public… |
| [Survival Analysis](https://openbiox.github.io/Bizard/Hiplot/169-survival.html) | data.table, ggplotify, jsonlite, survival, survminer | The survivorship curve is a graph showing the number or proportion of individuals surviving to each age for a given spec… |
| [Taylor Diagram](https://openbiox.github.io/Bizard/Hiplot/170-taylor-diagram.html) | openair | It can be used to display the standard deviation (SD), root mean square (RMS) error and correlation coefficient of the m… |
| [Time ROC](https://openbiox.github.io/Bizard/Hiplot/171-time-roc.html) | data.table, ggplot2, grid, jsonlite, plotROC, … | Receiver Operating Characteristic (ROC) analysis with time records in survival analysis. |
| [Treeheatr](https://openbiox.github.io/Bizard/Hiplot/172-treeheatr.html) | data.table, ggplotify, jsonlite, treeheatr | The heatmap decision tree is a visualization graph that combines two types of graphs: heatmap and decision tree visualiz… |
| [Treemap](https://openbiox.github.io/Bizard/Hiplot/173-treemap.html) | data.table, jsonlite, treemap | Tree map is a kind of tree structure diagram that graphical form to represent hierarchy structure. |
| [Tricolor Histogram](https://openbiox.github.io/Bizard/Hiplot/174-tricolor-histogram.html) | data.table, ggplot2, jsonlite | The tricolored histogram divides the histogram into three regions: low-value zone, middle-value zone, and high-value zon… |
| [UK Map](https://openbiox.github.io/Bizard/Hiplot/113-map-uk.html) | RColorBrewer, data.table, ggplot2, jsonlite | Create a UK Map using R with the Hiplot platform's approach. Suitable for biomedical data visualization with publication… |
| [UK Map (City)](https://openbiox.github.io/Bizard/Hiplot/112-map-uk-city.html) | RColorBrewer, data.table, ggplot2, jsonlite | Create a UK Map (City) using R with the Hiplot platform's approach. Suitable for biomedical data visualization with publ… |
| [UMAP](https://openbiox.github.io/Bizard/Hiplot/176-umap.html) | data.table, ggpubr, jsonlite, umap | UMAP is a nonlinear dimensionality reduction algorithm suitable for high-dimensional data reduction to two or three dime… |
| [USA Map (County)](https://openbiox.github.io/Bizard/Hiplot/114-map-usa-county.html) | RColorBrewer, data.table, ggplot2, jsonlite | Create a USA Map (County) using R with the Hiplot platform's approach. Suitable for biomedical data visualization with p… |
| [USA Map (States)](https://openbiox.github.io/Bizard/Hiplot/115-map-usa.html) | RColorBrewer, data.table, ggplot2, jsonlite | Create a USA Map (States) using R with the Hiplot platform's approach. Suitable for biomedical data visualization with p… |
| [Upset Plot](https://openbiox.github.io/Bizard/Hiplot/177-upset-plot.html) | ComplexHeatmap, VennDiagram, data.table, ggplot2, ggplotify, … | Upset can be used to show the interactive relationship between collections. |
| [Venn](https://openbiox.github.io/Bizard/Hiplot/178-venn.html) | VennDiagram, data.table, jsonlite | A Venn diagram is a diagramthat shows all possible logical relations between a finite collection of different sets. Thes… |
| [Venn2](https://openbiox.github.io/Bizard/Hiplot/179-venn2.html) | data.table, jsonlite, venn | A Venn diagram is a diagramthat shows all possible logical relations between a finite collection of different sets. Thes… |
| [Violin](https://openbiox.github.io/Bizard/Hiplot/181-violin.html) | data.table, ggpubr, ggthemes, jsonlite | The violin plot, named for its resemblance to a violin, is a statistical diagram combining a box diagram with a kernel d… |
| [Violin Group](https://openbiox.github.io/Bizard/Hiplot/180-violin-group.html) | data.table, ggpubr, ggthemes, jsonlite | Violin and box plot of grouped data with T-test. |
| [Visdat](https://openbiox.github.io/Bizard/Hiplot/182-visdat.html) | data.table, dplyr, ggplot2, jsonlite, patchwork, … | Create a Visdat using R with the Hiplot platform's approach. Suitable for biomedical data visualization with publication… |
| [Volcano](https://openbiox.github.io/Bizard/Hiplot/183-volcano.html) | data.table, ggpubr, jsonlite | The volcanogram is a visual representation of the difference in gene expression between two samples. |
| [Waffle Plot](https://openbiox.github.io/Bizard/Hiplot/184-waffle.html) | data.table, jsonlite, waffle | Create a Waffle Plot using R with the Hiplot platform's approach. Suitable for biomedical data visualization with public… |
| [Waterfalls](https://openbiox.github.io/Bizard/Hiplot/186-waterfalls.html) | data.table, ggplot2, jsonlite, waterfalls | The waterfall chart is used to display the cumulative effect of sequentially introduced positive or negative values . Th… |
| [Waterfalls Plot2](https://openbiox.github.io/Bizard/Hiplot/185-waterfalls-plot.html) | data.table, ggplot2, jsonlite, waterfalls | Used to visualize changes in data, with the difference from version 1 being the ability to customize the colors for upwa… |
| [World Map](https://openbiox.github.io/Bizard/Hiplot/116-map-world.html) | RColorBrewer, data.table, ggplot2, jsonlite | Create a World Map using R with the Hiplot platform's approach. Suitable for biomedical data visualization with publicat… |
| [World Map 2](https://openbiox.github.io/Bizard/Hiplot/117-map-world2.html) | RColorBrewer, data.table, ggplot2, jsonlite | Create a World Map 2 using R with the Hiplot platform's approach. Suitable for biomedical data visualization with public… |
| [ggwordcloud](https://openbiox.github.io/Bizard/Hiplot/076-ggwordcloud.html) | curl, data.table, ggwordcloud, jsonlite, png | The word cloud is to visualize the "keywords" that appear frequently in the web text by forming a "keyword cloud layer" … |
| [tSNE](https://openbiox.github.io/Bizard/Hiplot/175-tsne.html) | Rtsne, data.table, ggpubr, jsonlite | T-sne is a nonlinear dimensionality reduction algorithm suitable for high-dimensional data reduction to two or three dim… |

### Python

Python-based biomedical visualizations using matplotlib, seaborn, and plotnine.

**4 tutorials** | Languages: Python

| Tutorial | Packages | When to Use |
|----------|----------|-------------|
| [Heatmap (Python)](https://openbiox.github.io/Bizard/Python/Heatmap.html) | matplotlib, numpy, pandas, scipy, seaborn | A heatmap is a data visualization technique that uses color to represent values in a matrix. In biomedical research, hea… |
| [Scatter Plot (Python)](https://openbiox.github.io/Bizard/Python/ScatterPlot.html) | matplotlib, numpy, pandas, scipy, seaborn | A scatter plot displays values for two continuous variables as a collection of points. In biomedical research, scatter p… |
| [Violin Plot (Python)](https://openbiox.github.io/Bizard/Python/ViolinPlot.html) | matplotlib, numpy, pandas, seaborn | A violin plot combines a box plot and a kernel density estimation to show the distribution of continuous data across cat… |
| [Volcano Plot (Python)](https://openbiox.github.io/Bizard/Python/VolcanoPlot.html) | matplotlib, numpy, pandas | A volcano plot displays statistical significance (-log10 p-value) versus fold-change (log2 FC) for thousands of features… |

### Julia

Julia-based biomedical visualizations using CairoMakie and the Makie ecosystem.

**3 tutorials** | Languages: Julia

| Tutorial | Packages | When to Use |
|----------|----------|-------------|
| [Heatmap (Julia)](https://openbiox.github.io/Bizard/Julia/Heatmap.html) | CairoMakie | A heatmap visualizes matrix data using color gradients. Julia's `CairoMakie` provides high-performance heatmap rendering… |
| [Scatter Plot (Julia)](https://openbiox.github.io/Bizard/Julia/ScatterPlot.html) | CairoMakie, DataFrames | A scatter plot displays values for two continuous variables as a collection of points. Julia's `CairoMakie` package (par… |
| [Violin Plot (Julia)](https://openbiox.github.io/Bizard/Julia/ViolinPlot.html) | CairoMakie, DataFrames | A violin plot combines box plot statistics with kernel density estimation to show data distributions. Julia's `CairoMaki… |

---

## Key Packages by Language

### R

| Package | Used in # tutorials |
|---------|-------------------|
| `jsonlite` | 174 |
| `data.table` | 173 |
| `ggplot2` | 132 |
| `dplyr` | 50 |
| `ggplotify` | 33 |
| `RColorBrewer` | 32 |
| `ggpubr` | 25 |
| `tidyverse` | 21 |
| `viridis` | 19 |
| `cowplot` | 19 |
| `ggthemes` | 19 |
| `tidyr` | 16 |
| `hrbrthemes` | 14 |
| `patchwork` | 13 |
| `readr` | 12 |
| `stringr` | 9 |
| `survival` | 8 |
| `igraph` | 8 |
| `ggstatsplot` | 8 |
| `ComplexHeatmap` | 7 |

### Python

| Package | Used in # tutorials |
|---------|-------------------|
| `matplotlib` | 4 |
| `numpy` | 4 |
| `pandas` | 4 |
| `seaborn` | 3 |
| `scipy` | 2 |

### Julia

| Package | Used in # tutorials |
|---------|-------------------|
| `CairoMakie` | 3 |
| `DataFrames` | 2 |

---

## How to Use This Skill

1. **Identify the visualization need**: What data do you have? What story do you want to tell?
2. **Find the right category**: Browse the categories above to find relevant visualization types.
3. **Check the tutorial**: Click the tutorial link for full reproducible code and detailed explanation.
4. **Adapt the code**: Tutorials use public datasets — replace with your own data while keeping the same structure.

### Common Biomedical Use Cases

| Research Goal | Recommended Visualization | Category |
|--------------|--------------------------|----------|
| Compare gene expression across groups | Violin Plot, Box Plot | Distribution |
| Identify differentially expressed genes | Volcano Plot, MA Plot | Omics |
| Show survival outcomes | Kaplan-Meier Plot | Clinics |
| Explore gene correlations | Heatmap, Correlogram | Correlation |
| Display pathway enrichment | Enrichment Dot Plot, KEGG Plot | Omics |
| Show sample composition | Pie Chart, Treemap, Stacked Bar | Composition |
| Visualize genomic features | Circos Plot, Manhattan Plot | Omics |
| Track metrics over time | Line Chart, Area Plot | DataOverTime |
| Compare multiple groups | Grouped Bar, Radar Chart | Ranking |
| Show flow/transitions | Sankey Diagram, Alluvial Plot | Proportion |

---

## Supplementary Data

This skill package includes a gallery data table (`gallery_data.csv`) with 793 individual visualization examples, including image URLs, tutorial links, and descriptions for every figure in the atlas.

## License

CC-BY-NC — Bizard Collaboration Group, Luo Lab, and Wang Lab.
