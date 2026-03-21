# Skill: Streamgraph (R)

## Category
DataOverTime

## When to Use
A Streamgraph is a stacked area diagram. It represents the evolution of numerical variables across multiple groups. Typically, it displays areas around a central axis with rounded edges to create a flowing shape.

## Required R Packages
- dplyr
- ggplot2
- ggstream
- htmlwidgets
- streamgraph

## Minimal Reproducible Code
```r
streamgraph(covid_all, key = "location",
            value = "count",date = "time",
            height="300px", width="1000px")
```

## Full Tutorial
https://openbiox.github.io/Bizard/DataOverTime/Streamgraph.html
