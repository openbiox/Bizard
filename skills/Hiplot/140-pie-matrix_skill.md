# Skill: Pie Matrix (R)

## Category
Hiplot

## When to Use
Create a Pie Matrix using R with the Hiplot platform's approach. Suitable for biomedical data visualization with publication-quality output.

## Required R Packages
- data.table
- dplyr
- ggplot2
- jsonlite
- stringr
- tidyr

## Minimal Reproducible Code
```r
# Load packages
library(data.table)
library(dplyr)
library(ggplot2)
library(jsonlite)
library(stringr)
library(tidyr)

# Prepare data
# Load data
data <- data.table::fread(jsonlite::read_json("https://hiplot.cn/ui/basic/pie-matrix/data.json")$exampleData$textarea[[1]])
data <- as.data.frame(data)

# Convert data structure
data[,"genre"] <- factor(data[,"genre"], levels = unique(data[,"genre"]))
data[,"mpaa"] <- factor(data[,"mpaa"], levels = unique(data[,"mpaa"]))
data[,"status"] <- factor(data[,"status"], levels = unique(data[,"status"]))
col <- c("#E64B35FF","#4DBBD5FF")
df <- matrix(NA, nrow = length(unique(data[,"mpaa"])),
             ncol = length(unique(data[,"genre"])))
row.names(df) <- unique(data[,"mpaa"])
colnames(df) <- unique(data[,"genre"])
for (i in 1:nrow(df)) {
    for (j in 1:ncol(df)) {
      for (k in unique(data[,"status"])) {
        if (is.na(df[i, j])) {
          df[i, j] <- sum(data[,"genre"] == unique(data[,"genre"])[j] &
            data[,"mpaa"] == unique(data[,"mpaa"])[i] &
            data[,"status"] == k)
        } else {
          df[i, j] <- paste0(df[i, j], ",", 
            sum(data[,"genre"] == unique(data[,"genre"])[j] &
              data[,"mpaa"] == unique(data[,"mpaa"])[i] &
              data[,"status"] == k))
        }
      }
    }
}
df <- as.matrix(df)

# View data
head(data[,1:5])

# Create visualization
# Pie Matrix
p <- df %>% as.table() %>%
  as.data.frame() %>%
  mutate(Freq = str_split(Freq,",")) %>%
  unnest(Freq) %>%
  mutate(Freq = as.integer(Freq)) %>%
# ... (see full tutorial for more)
```

## Key Parameters
- `fill`: Maps `factor` to the fill aesthetic
- `width`: Controls element width
- `position`: Position adjustment (identity, dodge, stack, fill)
- `stat`: Statistical transformation to use
- `theme`: Plot theme; tutorial uses `theme_void()`

## Tips
- Customize color scales with `scale_fill_manual()` or `scale_color_brewer()`
- Use `facet_wrap()` or `facet_grid()` to create multi-panel plots by group
- See the full tutorial for additional customization options and advanced examples

## Full Tutorial
https://openbiox.github.io/Bizard/Hiplot/140-pie-matrix.html
