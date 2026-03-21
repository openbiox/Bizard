# Skill: Upset Plot (R)

## Category
Ranking

## When to Use
The Upset diagram is similar to the Venn diagram, mainly showing the number of elements in the intersection of different sets. However, when the number of sets in the Venn diagram reaches 5, the readability begins to drop sharply. The Upset diagram can well solve the problem of poor readability of the Venn diagram and can also provide additional statistical information on element properties.

## Required R Packages
- UpSetR
- ggupset

## Minimal Reproducible Code
```r
# Load packages
library(UpSetR)
library(ggupset)

# Prepare data
# UpSetR can accept three formats of data. The first is a list with named vectors (see listInput variable), the second is an expression vector (see expressionInput variable), and the third is a data frame consisting of 0,1 (see movies and mutations variables)
# Reading CSV data
# The list format requires each vector in the list to be a set. UpSetR requires each set to be named, and the elements in the vector are members of the corresponding set. When using the upset function to draw, you need to use the fromList function to convert the list data format.
listInput <- list(one = c(1, 2, 3, 5, 7, 8, 11, 12, 13), two = c(1, 2, 4, 5, 10), three = c(1, 5, 6, 7, 8, 9, 10, 12, 13))
# The expression format accepts a vector of expressions. The elements of the expression vector are the names of the sets in the intersection (separated by &), and the numeric elements in the intersection. When using the upset function to draw, you need to use the fromExpression function to convert the list data format.
expressionInput <- c(one = 2, two = 1, three = 2, `one&two` = 1, `one&three` = 4, `two&three` = 1, `one&two&three` = 2)

# In the data frame format, each column is a set and each row is an element. The data frame is required to consist of 0 and 1, which respectively indicate whether the element exists in the set. When a column has a value other than 0 or 1, the column is considered to be an attribute of the element.
movies <- read.csv( system.file("extdata", "movies.csv", package = "UpSetR"), header=T, sep=";" )
mutations <- read.csv( system.file("extdata", "mutations.csv", package = "UpSetR"), header=T, sep = ",")

# Create visualization
# Use the above three data types to draw the Upset graph
upset(fromList(listInput))
upset(fromExpression(expressionInput))
upset(movies)
```

## Key Parameters
- `fill`: Maps a variable to fill color for group comparison
- `color`: Maps a variable to outline/point color

## Tips
- The tutorial includes a '3. Advanced Upset Plot' section with advanced styling options
- Adjust text size with `theme(text = element_text(size = 14))` for presentations
- Sort categories by value rather than alphabetically for clearer ranking visualization

## Full Tutorial
https://openbiox.github.io/Bizard/Ranking/UpsetPlot.html
