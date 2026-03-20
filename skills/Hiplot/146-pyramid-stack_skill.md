# Skill: Pyramid Stack (R)

## Category
Hiplot

## When to use
::: callout-note
**Hiplot website**

## Required R packages
- data.table
- dplyr
- ggplot2
- ggthemes
- jsonlite

## Minimal reproducible code
```r
# Pyramid Stack
p <- ggplot(data = data, aes(x = age, y = pop, fill = year)) +
  geom_bar(data = data %>% filter(gender == "female") %>% arrange(rev(year)),
           stat = "identity", position = "identity") +
  geom_bar(data = data %>% filter(gender == "male") %>% arrange(rev(year)),
           stat = "identity", position = "identity", mapping = aes(y = -pop)) +
  coord_flip() +
  geom_hline(yintercept = 0) +
  scale_fill_economist() +
  scale_fill_manual(values = c("#e04d39","#5bbad6","#1e9f86")) +
  labs(y = "pop | male (left) - female (right)", x= "") +
  theme_economist(horizontal = FALSE) +
  theme(text = element_text(family = "Arial"),
        plot.title = element_text(size = 12,hjust = 0.5),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 10),
        axis.text.x = element_text(angle = 0, hjust = 0.5,vjust = 1),
        legend.position = "top",
        legend.direction = "horizontal",
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 10),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

p
```

## Full tutorial
https://openbiox.github.io/Bizard/Hiplot/146-pyramid-stack.html
