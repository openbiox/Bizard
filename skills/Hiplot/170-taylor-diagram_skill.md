# Skill: Taylor Diagram (R)

## Category
Hiplot

## When to Use
It can be used to display the standard deviation (SD), root mean square (RMS) error and correlation coefficient of the models simultaneously.

## Required R Packages
- openair

## Minimal Reproducible Code
```r
# Load packages
library(openair)

# Prepare data
# Load data
dat <- selectByDate(mydata, year = 2003)

# convert data structure
dat <- data.frame(date = mydata$date, obs = mydata$nox, mod = mydata$nox)
dat <- transform(dat, month = as.numeric(format(date, "%m")))
mod1 <- transform(dat, mod = mod + 10 * month + 10 * month * rnorm(nrow(dat)),
model = "model 1")
mod1 <- transform(mod1, mod = c(mod[5:length(mod)], mod[(length(mod) - 3) :
length(mod)]))
mod2 <- transform(dat, mod = mod + 7 * month + 7 * month * rnorm(nrow(dat)),
model = "model 2")
mod3 <- transform(dat, mod = mod + 3 * month + 3 * month * rnorm(nrow(dat)),
model = "model 3")
mod.dat <- rbind(mod1, mod2, mod3)

# View data
head(mod.dat)

# Create visualization
# Taylor Diagram
TaylorDiagram(mod.dat, obs = "obs", mod = "mod", group = "model",
              main = "Taylor diagram", 
              cols = c("#00468BFF","#8e6097","#BFACF0FF"))
```

## Key Parameters
- `fill`: Maps a variable to fill color for group comparison
- `color`: Maps a variable to outline/point color

## Tips
- Adjust text size with `theme(text = element_text(size = 14))` for presentations
- See the full tutorial for additional customization options and advanced examples

## Full Tutorial
https://openbiox.github.io/Bizard/Hiplot/170-taylor-diagram.html
