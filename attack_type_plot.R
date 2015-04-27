library(ggplot2)
library(RColorBrewer)

ggplot(data, aes(x=iyear, fill = attacktype1_txt)) + 
  geom_density(position="stack", aes(y = ..count..)) + 
  facet_wrap(~region_txt, ncol = 3) + 
  scale_fill_brewer(type = "div", palette = "Set3") + 
  guides(fill = guide_legend(reverse = TRUE)) +
  


