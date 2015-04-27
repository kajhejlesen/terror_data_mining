library(ggplot2)
library(plyr)

target_fatalities <- data.table(table(data$targtype1_txt, cut(data$nkill,c(-1,0,10,50,10000))))
target


ggplot(target_fatalities, aes(x = V1, y = N, fill = V2) ) +
  geom_bar(stat = "identity", position = "stack", aes(order=desc(V2)))
  
  