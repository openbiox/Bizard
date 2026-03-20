# Skill: Streamgraph (R)

## Category
DataOverTime

## When to use
A Streamgraph is a stacked area diagram. It represents the evolution of numerical variables across multiple groups. Typically, it displays areas around a central axis with rounded edges to create a flowing shape.

## Required R packages
- dplyr
- ggplot2
- ggstream
- htmlwidgets
- streamgraph

## Minimal reproducible code
```r
streamgraph(covid_all, key = "location",
            value = "count",date = "time",
            height="300px", width="1000px")
```

## Full tutorial
https://openbiox.github.io/Bizard/DataOverTime/Streamgraph.html
