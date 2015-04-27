library(ggplot2)
library(RColorBrewer)

ggplot(data, aes(x = iyear, fill = attacktype1_txt)) +
  geom_density(position = "stack", aes(y = ..count..)) + 
  facet_wrap(facets = ~region_txt, ncol = 3, scales = "free_y") +
  theme_bw() + 
  scale_fill_brewer(type = "div", palette = "Set3") +
  guides(fill = guide_legend(reverse = TRUE))

ggplot(subset(data, subset = country_txt == "Iraq"), aes(iyear, fill = attacktype1_txt)) +
  geom_density(position = "stack", aes(y = ..count..)) + 
  facet_wrap(facets = ~fatal, ncol = 2) + 
  guides(fill = guide_legend(reverse = TRUE))

ggplot()

